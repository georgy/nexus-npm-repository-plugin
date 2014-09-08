package com.bolyuba.nexus.plugin.npm.service.internal;

import java.io.IOException;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.annotation.Nullable;

import org.sonatype.nexus.proxy.ResourceStoreRequest;

import com.bolyuba.nexus.plugin.npm.service.PackageRoot;
import com.bolyuba.nexus.plugin.npm.service.PackageVersion;
import com.bolyuba.nexus.plugin.npm.service.ProxyMetadataService;
import com.bolyuba.nexus.plugin.npm.service.PackageRequest;
import com.bolyuba.nexus.plugin.npm.proxy.NpmProxyRepository;
import com.bolyuba.nexus.plugin.npm.service.tarball.TarballRequest;
import com.google.common.base.Function;
import com.google.common.collect.Maps;

import static com.google.common.base.Preconditions.checkNotNull;

/**
 * {@link ProxyMetadataService} implementation.
 */
public class ProxyMetadataServiceImpl
    extends GeneratorSupport
    implements ProxyMetadataService
{
  private static final String REGISTRY_ROOT_PACKAGE_NAME = "-";

  private static final String PROP_CACHED = "remote.cached";

  private static final String PROP_EXPIRED = "remote.expired";

  private final Object registryRootUpdateLock;

  private final NpmProxyRepository npmProxyRepository;

  private final MetadataStore metadataStore;

  private final MetadataGenerator metadataGenerator;

  private final ProxyMetadataTransport proxyMetadataTransport;

  public ProxyMetadataServiceImpl(final NpmProxyRepository npmProxyRepository,
                                  final MetadataStore metadataStore,
                                  final MetadataGenerator metadataGenerator,
                                  final ProxyMetadataTransport proxyMetadataTransport,
                                  final MetadataParser metadataParser)
  {
    super(metadataParser);
    this.registryRootUpdateLock = new Object();
    this.npmProxyRepository = checkNotNull(npmProxyRepository);
    this.metadataStore = checkNotNull(metadataStore);
    this.metadataGenerator = checkNotNull(metadataGenerator);
    this.proxyMetadataTransport = checkNotNull(proxyMetadataTransport);
  }

  @Override
  public boolean expireMetadataCaches(final PackageRequest request) {
    checkNotNull(request);
    if (request.isPackage()) {
      final PackageRoot packageRoot = metadataStore.getPackageByName(npmProxyRepository, request.getName());
      if (packageRoot == null) {
        return false;
      }
      log.info("Expiring package root {} in repository {}", request.getName(), npmProxyRepository.getId());
      packageRoot.getProperties().put(PROP_EXPIRED, Boolean.TRUE.toString());
      metadataStore.updatePackage(npmProxyRepository, packageRoot);
      return true;
    }
    else {
      final PackageRoot registryRoot = metadataStore.getPackageByName(npmProxyRepository, REGISTRY_ROOT_PACKAGE_NAME);
      if (registryRoot != null) {
        log.info("Expiring registry root of {}", npmProxyRepository.getId());
        registryRoot.getProperties().put(PROP_EXPIRED, Boolean.TRUE.toString());
        metadataStore.updatePackage(npmProxyRepository, registryRoot);
      }
      return 0 < metadataStore.updatePackages(npmProxyRepository, null, new Function<PackageRoot, PackageRoot>()
      {
        @Override
        public PackageRoot apply(@Nullable final PackageRoot input) {
          input.getProperties().put(PROP_EXPIRED, Boolean.TRUE.toString());
          return input;
        }
      });
    }
  }

  @Nullable
  @Override
  public PackageRoot generateRawPackageRoot(final String packageName) throws IOException {
    checkNotNull(packageName);
    return mayUpdatePackageRoot(packageName);
  }

  @Override
  public PackageRoot consumeRawPackageRoot(final PackageRoot packageRoot) {
    checkNotNull(packageRoot);
    return metadataStore.updatePackage(npmProxyRepository, packageRoot);
  }

  /**
   * Regex for tarball requests. They are in form of {@code /pkgName/-/pkgName-pkgVersion.tgz}, with a catch, that
   * pkgVersion might be suffixed by some suffix (ie. "beta", "alpha", etc). Groups in regexp: 1. the "packageName",
   * 2. the complete filename after "/-/"
   */
  private final static Pattern TARBALL_PATH_PATTERN = Pattern
      .compile("/([[a-z][A-Z][0-9]-_\\.]+)/-/([[a-z][A-Z][0-9]-_\\.]+\\.tgz)");

  @Nullable
  @Override
  public TarballRequest createTarballRequest(final ResourceStoreRequest request) throws IOException {
    checkNotNull(request);
    final Matcher matcher = TARBALL_PATH_PATTERN.matcher(request.getRequestPath());
    if (matcher.matches()) {
      final String packageName = matcher.group(1);
      final String tarballFilename = matcher.group(2);
      final PackageRoot packageRoot = mayUpdatePackageRoot(packageName);
      if (packageRoot != null) {
        log.debug("Looking up package {} version for tarball request: {}", packageRoot.getName(),
            request.getRequestPath());
        for (PackageVersion version : packageRoot.getVersions().values()) {
          // TODO: simpler regex with filename matching used for simplicity's sake. Version extracting might be less
          // robust due to complex regex
          if (version.getDistTarball().endsWith(tarballFilename)) {
            log.debug("Package {} version {} matched for tarball request: {}", packageRoot.getName(),
                version.getVersion(), request.getRequestPath());
            return new TarballRequest(request, packageRoot, version);
          }
        }
      }
    }
    log.debug("Not a tarball request: {}", request.getRequestPath());
    return null;
  }

  @Override
  protected PackageRootIterator doGenerateRegistryRoot(final PackageRequest request) throws IOException {
    // TODO: ContentServlet sets isLocal to paths ending with "/", so registry root will be local!
    if (request.getPath().endsWith("/") || !request.getStoreRequest().isRequestLocalOnly()) {
      // doing what NPM CLI does it's in own cache, using an invalid document (name "-" is invalid)
      PackageRoot registryRoot = metadataStore.getPackageByName(npmProxyRepository, REGISTRY_ROOT_PACKAGE_NAME);
      final long now = System.currentTimeMillis();
      if (registryRoot == null || isExpired(registryRoot, now)) {
        synchronized (registryRootUpdateLock) {
          // double checked locking, let's see again did some other thread update while we were blocked
          registryRoot = metadataStore.getPackageByName(npmProxyRepository, REGISTRY_ROOT_PACKAGE_NAME);
          if (registryRoot == null || isExpired(registryRoot, now)) {
            // fetch all from remote, this takes some time (currently 40MB JSON)
            if (registryRoot == null) {
              log.info("Initial NPM Registry root update for {}", npmProxyRepository.getId());
            }
            else {
              log.info("Expired NPM Registry root update for {}", npmProxyRepository.getId());
            }
            try (final PackageRootIterator packageRootIterator = proxyMetadataTransport
                .fetchRegistryRoot(npmProxyRepository)) {
              int count = metadataStore.updatePackages(npmProxyRepository, packageRootIterator);
              log.info("NPM Registry root updated {} packages for {}", count, npmProxyRepository.getId());
            }
            catch (Exception e) {
              // TODO: salvage as much as possible? As store commits per document anyway
              log.warn("NPM Registry root update failed for {}", npmProxyRepository.getId(), e);
            }
            if (registryRoot == null) {
              // create a fluke package root
              final Map<String, Object> versions = Maps.newHashMap();
              versions.put("0.0.0", "latest");
              final Map<String, Object> distTags = Maps.newHashMap();
              distTags.put("latest", "0.0.0");
              final Map<String, Object> raw = Maps.newHashMap();
              raw.put("name", REGISTRY_ROOT_PACKAGE_NAME);
              raw.put("description", "NX registry root package");
              raw.put("versions", versions);
              raw.put("dist-tags", distTags);
              registryRoot = new PackageRoot(npmProxyRepository.getId(), raw);
            }
            registryRoot.getProperties().put(PROP_EXPIRED, Boolean.FALSE.toString());
            registryRoot.getProperties().put(PROP_CACHED, Long.toString(now));
            metadataStore.updatePackage(npmProxyRepository, registryRoot);
          }
        }
      }
    }
    return metadataGenerator.generateRegistryRoot();
  }

  @Nullable
  @Override
  protected PackageRoot doGeneratePackageRoot(final PackageRequest request) throws IOException {
    if (!request.getStoreRequest().isRequestLocalOnly()) {
      if (mayUpdatePackageRoot(request.getName()) == null) {
        return null;
      }
    }
    return metadataGenerator.generatePackageRoot(request.getName());
  }

  @Nullable
  @Override
  protected PackageVersion doGeneratePackageVersion(final PackageRequest request) throws IOException {
    if (!request.getStoreRequest().isRequestLocalOnly()) {
      if (mayUpdatePackageRoot(request.getName()) == null) {
        return null;
      }
    }
    return metadataGenerator.generatePackageVersion(request.getName(), request.getVersion());
  }

  // ==

  /**
   * May fetch package root from remote if not found locally, or is found but is expired. The package root returned
   * document is NOT filtered, so this method should not be used to source documents sent downstream.
   */
  private PackageRoot mayUpdatePackageRoot(final String packageName) throws IOException {
    final long now = System.currentTimeMillis();
    PackageRoot packageRoot = metadataStore.getPackageByName(npmProxyRepository, packageName);
    if (packageRoot == null || isExpired(packageRoot, now)) {
      packageRoot = proxyMetadataTransport.fetchPackageRoot(npmProxyRepository, packageName, packageRoot);
      if (packageRoot == null) {
        return null;
      }
      packageRoot.getProperties().put(PROP_EXPIRED, Boolean.FALSE.toString());
      packageRoot.getProperties().put(PROP_CACHED, Long.toString(now));
      return metadataStore.updatePackage(npmProxyRepository, packageRoot);
    }
    else {
      return packageRoot;
    }
  }

  /**
   * Returns {@code true} if passed in package root should be considered as expired in cache, so remote check for fresh
   * version is required.
   */
  private boolean isExpired(final PackageRoot packageRoot, final long now) {
    if (!REGISTRY_ROOT_PACKAGE_NAME.equals(packageRoot.getName()) && packageRoot.isIncomplete()) {
      // registry root is made incomplete for simplicity's sake
      log.trace("EXPIRED: package {} is incomplete", packageRoot.getName());
      return true;
    }
    if (!npmProxyRepository.isItemAgingActive()) {
      log.trace("EXPIRED: package {} owning repository item aging is inactive", packageRoot.getName());
      return true;
    }
    if (Boolean.TRUE.toString().equals(packageRoot.getProperties().get(PROP_EXPIRED))) {
      log.trace("EXPIRED: package {} flagged as expired", packageRoot.getName());
      return true;
    }
    if (npmProxyRepository.getItemMaxAge() < 0) {
      log.trace("NOT-EXPIRED: package {} owning repository {} has negative item max age", packageRoot.getName(),
          npmProxyRepository.getId());
      return false;
    }
    final long remoteCached = packageRoot.getProperties().containsKey(PROP_CACHED) ? Long
        .valueOf(packageRoot.getProperties().get(PROP_CACHED)) : now;
    final boolean result = ((now - remoteCached) > (npmProxyRepository.getItemMaxAge() * 60L * 1000L));
    if (result) {
      log.trace("EXPIRED: package {} is too old", packageRoot.getName());
    }
    else {
      log.trace("NOT-EXPIRED: package {} is fresh", packageRoot.getName());
    }
    return result;
  }
}

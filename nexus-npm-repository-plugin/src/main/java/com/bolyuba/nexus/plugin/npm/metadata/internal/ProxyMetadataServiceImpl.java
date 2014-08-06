package com.bolyuba.nexus.plugin.npm.metadata.internal;

import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.util.Map;

import javax.annotation.Nullable;

import org.sonatype.nexus.proxy.item.ContentLocator;
import org.sonatype.nexus.proxy.item.FileContentLocator;
import org.sonatype.nexus.proxy.item.PreparedContentLocator;
import org.sonatype.nexus.proxy.storage.remote.httpclient.HttpClientManager;

import com.bolyuba.nexus.plugin.npm.NpmRepository;
import com.bolyuba.nexus.plugin.npm.metadata.PackageRoot;
import com.bolyuba.nexus.plugin.npm.metadata.PackageVersion;
import com.bolyuba.nexus.plugin.npm.metadata.ProxyMetadataService;
import com.bolyuba.nexus.plugin.npm.pkg.PackageRequest;
import com.bolyuba.nexus.plugin.npm.proxy.NpmProxyRepository;
import com.google.common.base.Function;
import com.google.common.collect.Maps;
import org.apache.http.HttpResponse;
import org.apache.http.HttpStatus;
import org.apache.http.client.HttpClient;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.util.EntityUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import static com.google.common.base.Preconditions.checkNotNull;

/**
 * {@link ProxyMetadataService} implementation.
 */
public class ProxyMetadataServiceImpl
    extends GeneratorSupport
    implements ProxyMetadataService
{
  private static final String REGISTRY_ROOT_PACKAGE_NAME = "-";

  private static final String PROP_ETAG = "remote.etag";

  private static final String PROP_CACHED = "remote.cached";

  private static final String PROP_EXPIRED = "remote.expired";

  private static final Logger outboundRequestLog = LoggerFactory.getLogger("remote.storage.outbound");

  private final NpmProxyRepository npmProxyRepository;

  private final HttpClientManager httpClientManager;

  private final File temporaryDirectory;

  private final MetadataStore metadataStore;

  private final MetadataGenerator metadataGenerator;

  private final MetadataParser metadataParser;

  public ProxyMetadataServiceImpl(final NpmProxyRepository npmProxyRepository,
                                  final HttpClientManager httpClientManager,
                                  final File temporaryDirectory,
                                  final MetadataStore metadataStore,
                                  final MetadataGenerator metadataGenerator,
                                  final MetadataParser metadataParser)
  {
    super(metadataParser);
    this.npmProxyRepository = checkNotNull(npmProxyRepository);
    this.httpClientManager = checkNotNull(httpClientManager);
    this.temporaryDirectory = checkNotNull(temporaryDirectory);
    this.metadataStore = checkNotNull(metadataStore);
    this.metadataGenerator = checkNotNull(metadataGenerator);
    this.metadataParser = checkNotNull(metadataParser);
  }

  @Override
  public boolean expireMetadataCaches(final PackageRequest request) {
    if (request.isPackage()) {
      final PackageRoot packageRoot = metadataStore.getPackageByName(npmProxyRepository, request.getName());
      if (packageRoot == null) {
        return false;
      }
      packageRoot.getProperties().put(PROP_EXPIRED, Boolean.TRUE.toString());
      metadataStore.updatePackage(npmProxyRepository, packageRoot);
      return true;
    }
    else {
      final PackageRoot registryRoot = metadataStore.getPackageByName(npmProxyRepository, REGISTRY_ROOT_PACKAGE_NAME);
      if (registryRoot != null) {
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

  @Override
  protected PackageRootIterator doGenerateRegistryRoot(final PackageRequest request) throws IOException {
    // TODO: ContentServlet sets isLocal to paths ending with "/", so registry root will be local!
    if (request.getPath().endsWith("/") || !request.getStoreRequest().isRequestLocalOnly()) {
      // doing what NPM CLI does it's in own cache, using an invalid document (name "-" is invalid)
      PackageRoot registryRoot = metadataStore.getPackageByName(npmProxyRepository, REGISTRY_ROOT_PACKAGE_NAME);
      final long now = System.currentTimeMillis();
      if (registryRoot == null || isExpired(registryRoot, now)) {
        fetchRegistryRoot(); // fetch all
        if (registryRoot == null) {
          // create a fluke package root
          final Map<String, Object> versions = Maps.newHashMap();
          versions.put("0.0.0", "latest");
          final Map<String, Object> raw = Maps.newHashMap();
          raw.put("name", REGISTRY_ROOT_PACKAGE_NAME);
          raw.put("description", "NX registry root package");
          raw.put("versions", versions);
          registryRoot = new PackageRoot(npmProxyRepository.getId(), raw);
        }
        registryRoot.getProperties().put(PROP_EXPIRED, Boolean.FALSE.toString());
        registryRoot.getProperties().put(PROP_CACHED, Long.toString(now));
        metadataStore.updatePackage(npmProxyRepository, registryRoot);
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
   * May fetch package root from remote if not found locally, or is found but is expired.
   */
  private PackageRoot mayUpdatePackageRoot(final String packageName) throws IOException {
    final long now = System.currentTimeMillis();
    PackageRoot packageRoot = metadataGenerator.generatePackageRoot(packageName);
    if (packageRoot == null || isExpired(packageRoot, now)) {
      packageRoot = fetchPackageRoot(packageName, packageRoot);
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
    if (packageRoot.isIncomplete()) {
      return true;
    }
    if (!npmProxyRepository.isItemAgingActive()) {
      return true;
    }
    if (Boolean.TRUE.toString().equals(packageRoot.getProperties().get(PROP_EXPIRED))) {
      return true;
    }
    if (npmProxyRepository.getItemMaxAge() < 0) {
      return false;
    }
    final long remoteCached = packageRoot.getProperties().containsKey(PROP_CACHED) ? Long
        .valueOf(packageRoot.getProperties().get(PROP_CACHED)) : now;
    return ((now - remoteCached) > (npmProxyRepository.getItemMaxAge() * 60L * 1000L));
  }

  /**
   * Performs a HTTP GET to fetch the registry root. Note: by testing on my mac (MBP 2012 SSD), seems OrientDB is "slow"
   * to consume the streamed HTTP response (ie. to push it immediately into database, maintaining indexes etc). Hence,
   * we save the response JSON to temp file and parse it from there to not have remote registry HTTP Server give up
   * on connection with us.
   */
  private int fetchRegistryRoot() throws IOException {
    final HttpClient httpClient = httpClientManager.create(npmProxyRepository,
        npmProxyRepository.getRemoteStorageContext());
    try {
      final HttpGet get = new HttpGet(buildUri("-/all")); // TODO: this in NPM specific, might try both root and NPM api
      log.info("NPM Registry root update for {}", npmProxyRepository.getId());
      // TODO: during devel INFO, should be DEBUG
      outboundRequestLog.info("{} - NPM GET {}", npmProxyRepository.getId(), get.getURI());
      get.addHeader("accept", NpmRepository.JSON_MIME_TYPE);
      final HttpResponse httpResponse = httpClient.execute(get);
      try {
        // TODO: during devel INFO, should be DEBUG
        outboundRequestLog.info("{} - NPM GET {} - {}", npmProxyRepository.getId(), get.getURI(),
            httpResponse.getStatusLine());
        if (httpResponse.getStatusLine().getStatusCode() == HttpStatus.SC_OK) {
          final File tempFile = File
              .createTempFile(npmProxyRepository.getId() + "-root", "temp.json", temporaryDirectory);
          try (final BufferedOutputStream bos = new BufferedOutputStream(new FileOutputStream(tempFile))) {
            httpResponse.getEntity().writeTo(bos);
            bos.flush();
          }
          try {
            final FileContentLocator cl = new FileContentLocator(tempFile, NpmRepository.JSON_MIME_TYPE);
            try (final PackageRootIterator roots = metadataParser.parseRegistryRoot(npmProxyRepository.getId(), cl)) {
              try {
                int count = metadataStore.updatePackages(npmProxyRepository, roots);
                log.info("NPM Registry root updated {} packages for {}", count, npmProxyRepository.getId());
                return count;
              } catch (Exception e) {
                // TODO: salvage as much as possible?
                log.warn("NPM Registry root update failed for {}", npmProxyRepository.getId(), e);
                return -1;
              }
            }
          }
          finally {
            tempFile.delete();
          }
        }
        throw new IOException("Unexpected response from registry root " + httpResponse.getStatusLine());
      }
      finally {
        EntityUtils.consumeQuietly(httpResponse.getEntity());
      }
    }
    finally {
      httpClientManager.release(npmProxyRepository, npmProxyRepository.getRemoteStorageContext());
    }
  }

  /**
   * Performs a conditional GET to fetch the package root and returns the fetched package root. If fetch succeeded
   * (HTTP 200 Ok is returned), the package root is also pushed into {@code MetadataStore}. In short, the returned
   * package root from this method is guaranteed to be present in the store too.
   */
  private PackageRoot fetchPackageRoot(final String packageName, final PackageRoot expired) throws IOException {
    final HttpClient httpClient = httpClientManager.create(npmProxyRepository,
        npmProxyRepository.getRemoteStorageContext());
    try {
      final HttpGet get = new HttpGet(buildUri(packageName));
      // TODO: during devel INFO, should be DEBUG
      outboundRequestLog.info("{} - NPM GET {}", npmProxyRepository.getId(), get.getURI());
      get.addHeader("accept", NpmRepository.JSON_MIME_TYPE);
      if (expired != null && expired.getProperties().containsKey(PROP_ETAG)) {
        get.addHeader("if-none-match", expired.getProperties().get(PROP_ETAG));
      }
      final HttpResponse httpResponse = httpClient.execute(get);
      try {
        // TODO: during devel INFO, should be DEBUG
        outboundRequestLog.info("{} - NPM GET {} - {}", npmProxyRepository.getId(), get.getURI(),
            httpResponse.getStatusLine());
        if (httpResponse.getStatusLine().getStatusCode() == HttpStatus.SC_NOT_MODIFIED) {
          return expired;
        }
        if (httpResponse.getStatusLine().getStatusCode() == HttpStatus.SC_OK) {
          final PreparedContentLocator pcl = new PreparedContentLocator(httpResponse.getEntity().getContent(),
              NpmRepository.JSON_MIME_TYPE, ContentLocator.UNKNOWN_LENGTH);
          final PackageRoot fresh = metadataParser.parsePackageRoot(npmProxyRepository.getId(), pcl);
          if (httpResponse.containsHeader("etag")) {
            fresh.getProperties().put(PROP_ETAG, httpResponse.getFirstHeader("etag").getValue());
          }
          return fresh;
        }
        return null;
      }
      finally {
        EntityUtils.consumeQuietly(httpResponse.getEntity());
      }
    }
    finally {
      httpClientManager.release(npmProxyRepository, npmProxyRepository.getRemoteStorageContext());
    }
  }

  /**
   * Builds and return registry URI for given package name.
   */
  private String buildUri(final String pathElem) {
    final String registryUrl = npmProxyRepository.getRemoteUrl();
    if (registryUrl.endsWith("/")) {
      return registryUrl + pathElem;
    }
    else {
      return registryUrl + "/" + pathElem;
    }
  }
}

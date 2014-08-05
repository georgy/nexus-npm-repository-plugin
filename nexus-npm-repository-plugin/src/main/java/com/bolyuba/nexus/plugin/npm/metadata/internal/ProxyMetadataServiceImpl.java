package com.bolyuba.nexus.plugin.npm.metadata.internal;

import java.io.IOException;
import java.util.List;

import javax.annotation.Nullable;

import org.sonatype.nexus.proxy.item.ContentLocator;
import org.sonatype.nexus.proxy.item.PreparedContentLocator;
import org.sonatype.nexus.proxy.storage.remote.httpclient.HttpClientManager;

import com.bolyuba.nexus.plugin.npm.NpmRepository;
import com.bolyuba.nexus.plugin.npm.metadata.PackageRoot;
import com.bolyuba.nexus.plugin.npm.metadata.ProxyMetadataService;
import com.bolyuba.nexus.plugin.npm.metadata.internal.MetadataParser.PackageRootIterator;
import com.bolyuba.nexus.plugin.npm.pkg.PackageRequest;
import com.bolyuba.nexus.plugin.npm.proxy.NpmProxyRepository;
import com.google.common.base.Function;
import org.apache.http.HttpResponse;
import org.apache.http.HttpStatus;
import org.apache.http.client.HttpClient;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.util.EntityUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import static com.google.common.base.Preconditions.checkArgument;
import static com.google.common.base.Preconditions.checkNotNull;

/**
 * {@link ProxyMetadataService} implementation.
 */
public class ProxyMetadataServiceImpl
    implements ProxyMetadataService
{
  private static final String PROP_ETAG = "remote.etag";

  private static final String PROP_CACHED = "remote.cached";

  private static final String PROP_EXPIRED = "remote.expired";

  private static final Logger outboundRequestLog = LoggerFactory.getLogger("remote.storage.outbound");

  private final NpmProxyRepository npmProxyRepository;

  private final HttpClientManager httpClientManager;

  private final MetadataStore metadataStore;

  private final MetadataParser metadataParser;

  private final MetadataProducer metadataProducer;

  public ProxyMetadataServiceImpl(final NpmProxyRepository npmProxyRepository,
                                  final HttpClientManager httpClientManager,
                                  final MetadataStore metadataStore,
                                  final MetadataParser metadataParser,
                                  final MetadataProducer metadataProducer)
  {
    this.npmProxyRepository = checkNotNull(npmProxyRepository);
    this.httpClientManager = checkNotNull(httpClientManager);
    this.metadataStore = checkNotNull(metadataStore);
    this.metadataParser = checkNotNull(metadataParser);
    this.metadataProducer = checkNotNull(metadataProducer);
  }

  @Override
  public boolean expireMetadataCaches(final PackageRequest request) {
    // TODO: obey package name, version narrowing
    return 0 < metadataStore.updatePackages(npmProxyRepository, null, new Function<PackageRoot, PackageRoot>()
    {
      @Override
      public PackageRoot apply(@Nullable final PackageRoot input) {
        input.getProperties().put(PROP_EXPIRED, Boolean.TRUE.toString());
        return input;
      }
    });
  }

  @Override
  public ContentLocator produceRegistryRoot(final PackageRequest packageRequest) throws IOException {
    if (!packageRequest.getStoreRequest().isRequestLocalOnly()) {
      final List<String> packageNames = metadataStore.listPackageNames(npmProxyRepository);
      if (packageNames.isEmpty()) {
        fetchRegistryRoot();
        // TODO: expire when needed all packages? When to refetch?
      }
    }
    return metadataProducer.produceRegistryRoot();
  }

  @Nullable
  @Override
  public ContentLocator producePackageRoot(final PackageRequest packageRequest) throws IOException {
    checkArgument(packageRequest.isPackageRoot(), "Package root request expected, but got %s",
        packageRequest.getPath());
    if (!packageRequest.getStoreRequest().isRequestLocalOnly()) {
      if (mayUpdatePackageRoot(packageRequest.getName()) == null) {
        return null;
      }
    }
    return metadataProducer.producePackageRoot(packageRequest.getName());
  }

  @Nullable
  @Override
  public ContentLocator producePackageVersion(final PackageRequest packageRequest)
      throws IOException
  {
    checkArgument(packageRequest.isPackageVersion(), "Package version request expected, but got %s",
        packageRequest.getPath());
    if (!packageRequest.getStoreRequest().isRequestLocalOnly()) {
      if (mayUpdatePackageRoot(packageRequest.getName()) == null) {
        return null;
      }
    }
    return metadataProducer.producePackageVersion(packageRequest.getName(), packageRequest.getVersion());
  }

  // ==

  /**
   * May fetch package root from remote if not found locally, or is found but is expired.
   */
  private PackageRoot mayUpdatePackageRoot(final String packageName) throws IOException {
    final long now = System.currentTimeMillis();
    PackageRoot packageRoot = metadataStore.getPackageByName(npmProxyRepository, packageName);
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
   * Performs a HTTP GET to fetch the registry root.
   */
  private int fetchRegistryRoot() throws IOException {
    final HttpClient httpClient = httpClientManager.create(npmProxyRepository,
        npmProxyRepository.getRemoteStorageContext());
    try {
      final HttpGet get = new HttpGet(buildUri("-/all")); // TODO: this in NPM specific, might try both root and NPM api
      // TODO: during devel INFO, should be DEBUG
      outboundRequestLog.info("{} - NPM GET {}", npmProxyRepository.getId(), get.getURI());
      get.addHeader("accept", NpmRepository.JSON_MIME_TYPE);
      final HttpResponse httpResponse = httpClient.execute(get);
      try {
        // TODO: during devel INFO, should be DEBUG
        outboundRequestLog.info("{} - NPM GET {} - {}", npmProxyRepository.getId(), get.getURI(),
            httpResponse.getStatusLine());
        if (httpResponse.getStatusLine().getStatusCode() == HttpStatus.SC_OK) {
          final PreparedContentLocator pcl = new PreparedContentLocator(httpResponse.getEntity().getContent(),
              NpmRepository.JSON_MIME_TYPE, ContentLocator.UNKNOWN_LENGTH);
          try (final PackageRootIterator roots = metadataParser.parseRegistryRoot(pcl)) {
            return metadataStore.updatePackages(npmProxyRepository, roots);
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
          final PackageRoot fresh = metadataParser.parsePackageRoot(pcl);
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

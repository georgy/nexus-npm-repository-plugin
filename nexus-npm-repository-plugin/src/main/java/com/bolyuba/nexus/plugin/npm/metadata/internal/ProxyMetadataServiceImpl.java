package com.bolyuba.nexus.plugin.npm.metadata.internal;

import java.io.IOException;
import java.util.List;

import javax.annotation.Nullable;

import org.sonatype.nexus.proxy.ResourceStoreRequest;
import org.sonatype.nexus.proxy.item.ContentLocator;
import org.sonatype.nexus.proxy.item.PreparedContentLocator;
import org.sonatype.nexus.proxy.storage.remote.httpclient.HttpClientManager;

import com.bolyuba.nexus.plugin.npm.NpmRepository;
import com.bolyuba.nexus.plugin.npm.metadata.PackageRoot;
import com.bolyuba.nexus.plugin.npm.metadata.ProxyMetadataService;
import com.bolyuba.nexus.plugin.npm.proxy.NpmProxyRepository;
import com.google.common.annotations.VisibleForTesting;
import org.apache.http.HttpResponse;
import org.apache.http.HttpStatus;
import org.apache.http.client.HttpClient;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.util.EntityUtils;

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

  @VisibleForTesting
  public MetadataParser getMetadataParser() {
    return metadataParser;
  }

  @Override
  public boolean expireMetadataCaches(final ResourceStoreRequest request) {
    // TODO: flip the PROP_EXPIRED prop on selected package roots
    return false;
  }

  @Override
  public ContentLocator produceRegistryRoot() throws IOException {
    final List<String> packageNames = metadataStore.listPackageNames(npmProxyRepository);
    if (packageNames.isEmpty()) {
      // TODO: fetch and update all packages
    }
    return metadataProducer.produceRegistryRoot();
  }

  @Nullable
  @Override
  public ContentLocator producePackageRoot(final String packageName) throws IOException {
    if (mayUpdatePackageRoot(packageName) == null) {
      return null;
    }
    return metadataProducer.producePackageRoot(packageName);
  }

  @Nullable
  @Override
  public ContentLocator producePackageVersion(final String packageName, final String packageVersion)
      throws IOException
  {
    if (mayUpdatePackageRoot(packageName) == null) {
      return null;
    }
    return metadataProducer.producePackageVersion(packageName, packageVersion);
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
   * Performs a conditional GET to fetch the package root and returns the fetched package root. If fetch succeeded
   * (HTTP 200 Ok is returned), the package root is also pushed into {@code MetadataStore}. In short, the returned
   * package root from this method is guaranteed to be present in the store too.
   */
  protected PackageRoot fetchPackageRoot(final String packageName, final PackageRoot expired) throws IOException {
    final HttpClient httpClient = httpClientManager.create(npmProxyRepository,
        npmProxyRepository.getRemoteStorageContext());
    try {
      final HttpGet get = new HttpGet(npmProxyRepository.getRemoteUrl() + packageName);
      get.addHeader("accept", NpmRepository.JSON_MIME_TYPE);
      if (expired != null && expired.getProperties().containsKey(PROP_ETAG)) {
        get.addHeader("if-none-match", expired.getProperties().get(PROP_ETAG));
      }
      final HttpResponse httpResponse = httpClient.execute(get);
      try {
        if (httpResponse.getStatusLine().getStatusCode() == HttpStatus.SC_NOT_MODIFIED) {
          return expired;
        }
        if (httpResponse.getStatusLine().getStatusCode() == HttpStatus.SC_OK) {
          // TODO: NPM registry does not tell the content-type!
          // TODO: NPM registry does not tell the content-length!
          // TODO: WTF?
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
}

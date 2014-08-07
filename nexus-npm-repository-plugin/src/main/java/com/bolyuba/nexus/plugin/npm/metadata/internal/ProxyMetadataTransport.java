package com.bolyuba.nexus.plugin.npm.metadata.internal;

import java.io.IOException;

import com.bolyuba.nexus.plugin.npm.metadata.PackageRoot;
import com.bolyuba.nexus.plugin.npm.proxy.NpmProxyRepository;

/**
 * Transport for NPM Metadata.
 */
public interface ProxyMetadataTransport
{
  /**
   * Fetches remote registry root of the proxied {@link NpmProxyRepository}. The returned iterator MUST BE handled as
   * resource, as it incrementally parsing a potentially huge JSON document!
   */
  PackageRootIterator fetchRegistryRoot(final NpmProxyRepository npmProxyRepository) throws IOException;

  /**
   * Fetches one single package root of the proxied {@link NpmProxyRepository}.
   */
  PackageRoot fetchPackageRoot(final NpmProxyRepository npmProxyRepository, final String packageName,
                               final PackageRoot expired) throws IOException;
}

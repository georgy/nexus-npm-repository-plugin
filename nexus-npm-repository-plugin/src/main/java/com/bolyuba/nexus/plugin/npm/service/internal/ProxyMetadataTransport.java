package com.bolyuba.nexus.plugin.npm.service.internal;

import java.io.IOException;

import javax.annotation.Nullable;

import com.bolyuba.nexus.plugin.npm.proxy.NpmProxyRepository;
import com.bolyuba.nexus.plugin.npm.service.PackageRoot;

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
   * Fetches one single package root of the proxied {@link NpmProxyRepository}. Supplied repository and package name
   * must not be {@code null}s, while the expired packageRoot might be {@code null}. If present, metadata from it
   * like ETag will be used to make a "conditional GET", and if remote unchanged, the passed in expired instance
   * is returned. If package not found or cannot be fetched for any reason, {@code null} is returned.
   */
  @Nullable
  PackageRoot fetchPackageRoot(final NpmProxyRepository npmProxyRepository, final String packageName,
                               final @Nullable PackageRoot expired) throws IOException;
}

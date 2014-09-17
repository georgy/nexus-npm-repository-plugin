package com.bolyuba.nexus.plugin.npm.service;

import java.io.IOException;

import javax.annotation.Nullable;

import org.sonatype.nexus.proxy.ResourceStoreRequest;

import com.bolyuba.nexus.plugin.npm.service.tarball.TarballRequest;

/**
 * Metadata service for proxy repositories. Component generating NPM metadata from underlying store to
 * be sent downstream for consumption by NPM CLI or alike. Still, unlike "plain" generator, this
 * component ensures first that the store contains up to date data and serves that.
 */
public interface ProxyMetadataService
    extends Generator
{
  /**
   * Expires proxy metadata cache. On next request of an expired metadata, re-fetch will be done from registry.
   */
  boolean expireMetadataCaches(PackageRequest request);

  /**
   * Updates package root in metadata store. To be used mostly by proxy mechanism to store some extra properties,
   * not to modify actual metadata.
   */
  PackageRoot consumeRawPackageRoot(PackageRoot packageRoot) throws IOException;

  /**
   * Creates a {@link TarballRequest} out of a {@link ResourceStoreRequest}, if applicable. It relies on metadata store
   * to find out what package and what version of it is requested. If the request does not meet formal criteria (ie.
   * request path is not for a tarball request), or no package or corresponding version is found, {@code null} is
   * returned to signal that request is not a tarball request.
   */
  @Nullable
  TarballRequest createTarballRequest(ResourceStoreRequest request) throws IOException;
}

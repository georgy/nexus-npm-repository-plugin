package com.bolyuba.nexus.plugin.npm.metadata;

import com.bolyuba.nexus.plugin.npm.pkg.PackageRequest;

/**
 * Metadata service for proxy repositories. Component producing "raw" (streamed) NPM metadata from underlying store to
 * be sent downstream for consumption by NPM CLI or alike. Still, unlike "plain" producer, this
 * component ensures first that the store contains up to date data and serves that.
 */
public interface ProxyMetadataService
    extends Generator, Producer
{
  /**
   * Expires proxy metadata cache. On next request of an expired metadata, refetch will be done from registry.
   */
  boolean expireMetadataCaches(PackageRequest request);
}

package com.bolyuba.nexus.plugin.npm.metadata;

import java.io.IOException;

import javax.annotation.Nullable;

import org.sonatype.nexus.proxy.item.ContentLocator;

import com.bolyuba.nexus.plugin.npm.pkg.PackageRequest;

/**
 * Metadata produces that produces "raw", probably streamed output from underlyng metadata store.
 */
public interface Producer
{
  ContentLocator produceRegistryRoot(PackageRequest request) throws IOException;

  @Nullable
  ContentLocator producePackageRoot(PackageRequest request) throws IOException;

  @Nullable
  ContentLocator producePackageVersion(PackageRequest request) throws IOException;
}


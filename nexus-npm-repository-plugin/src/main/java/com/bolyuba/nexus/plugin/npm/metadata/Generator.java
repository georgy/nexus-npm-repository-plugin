package com.bolyuba.nexus.plugin.npm.metadata;

import java.io.IOException;

import javax.annotation.Nullable;

import com.bolyuba.nexus.plugin.npm.metadata.internal.PackageRootIterator;
import com.bolyuba.nexus.plugin.npm.pkg.PackageRequest;

/**
 * Metadata generator that generates entities to be sent downstream to clint.
 */
public interface Generator
{
  /**
   * Returns {@link Producer} backed by this generator.
   */
  Producer getProducer();

  /**
   * Returns registry root package iterator. Note: these package roots are "shrinked" and should not be used
   * in any other case. Never returns {@code null}, but iterator might be empty.
   *
   * @see PackageRoot#shrinkToLatestVersionOnly()
   */
  PackageRootIterator generateRegistryRoot(PackageRequest request) throws IOException;

  /**
   * Returns corresponding package root for given request, or {@link null} if no such package.
   */
  @Nullable
  PackageRoot generatePackageRoot(PackageRequest request) throws IOException;

  /**
   * Returns corresponding package version for given request, or {@link null} if no such version.
   */
  @Nullable
  PackageVersion generatePackageVersion(PackageRequest request) throws IOException;
}


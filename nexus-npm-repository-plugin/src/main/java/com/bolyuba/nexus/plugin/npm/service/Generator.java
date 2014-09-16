package com.bolyuba.nexus.plugin.npm.service;

import java.io.IOException;

import javax.annotation.Nullable;

import org.sonatype.nexus.proxy.item.ContentLocator;

import com.bolyuba.nexus.plugin.npm.service.internal.PackageRootIterator;

/**
 * Metadata generator that generates entities to be sent downstream to clint.
 */
public interface Generator
{
  /**
   * Produces "raw" downstream content of registry root.
   */
  ContentLocator produceRegistryRoot(PackageRequest request) throws IOException;

  /**
   * Produces "raw" downstream content of package root, or {@code null} if no given package found.
   */
  @Nullable
  ContentLocator producePackageRoot(PackageRequest request) throws IOException;

  /**
   * Produces "raw" downstream content of package version, or {@code null} if no given version of package or containing
   * package not found.
   */
  @Nullable
  ContentLocator producePackageVersion(PackageRequest request) throws IOException;

  /**
   * Returns registry root package iterator. Note: these package roots are "shrinked" and should not be used
   * in any other case. Never returns {@code null}, but iterator might be empty.
   *
   * @see PackageRoot#shrinkPackageVersions()
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


package com.bolyuba.nexus.plugin.npm.service;

import java.io.IOException;

import javax.annotation.Nullable;

import org.sonatype.nexus.proxy.ResourceStoreRequest;
import org.sonatype.nexus.proxy.item.ContentLocator;

import com.bolyuba.nexus.plugin.npm.service.internal.PackageRootIterator;

/**
 * Metadata generator that generates entities to be sent downstream to clint.
 */
public interface Generator
{
  /**
   * Returns {@code true} if request or some setting is not disabling the NPM metadata service for given request.
   */
  boolean isNpmMetadataServiced(ResourceStoreRequest request);

  /**
   * Produces "raw" downstream content of registry root. Meant for downstream consumption, as package JSON document is
   * properly altered (ie. tarball URLs).
   */
  ContentLocator produceRegistryRoot(PackageRequest request) throws IOException;

  /**
   * Produces "raw" downstream content of package root. Meant for downstream consumption, as package JSON document is
   * properly altered (ie. tarball URLs). If no package found for request, returns {@code null}.
   */
  @Nullable
  ContentLocator producePackageRoot(PackageRequest request) throws IOException;

  /**
   * Produces "raw" downstream content of package root. Meant for downstream consumption, as package JSON document is
   * properly altered (ie. tarball URLs). If no package found or package has no given version, returns {@code null}.
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


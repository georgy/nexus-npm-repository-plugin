package com.bolyuba.nexus.plugin.npm.metadata;

import java.io.IOException;

import javax.annotation.Nullable;

import org.sonatype.nexus.proxy.item.ContentLocator;

/**
 * Metadata service for hosted repositories, it serves up what has been consumed by it (deployed to it).
 */
public interface HostedMetadataService
{
  int consumeRegistryRoot(final ContentLocator contentLocator) throws IOException;

  PackageRoot consumePackageRoot(final ContentLocator contentLocator) throws IOException;

  ContentLocator produceRegistryRoot() throws IOException;

  @Nullable
  ContentLocator producePackageRoot(String packageName) throws IOException;

  @Nullable
  ContentLocator producePackageVersion(String packageName, String packageVersion) throws IOException;
}

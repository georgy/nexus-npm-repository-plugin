package com.bolyuba.nexus.plugin.npm.metadata;

import java.io.IOException;

import javax.annotation.Nullable;

import org.sonatype.nexus.proxy.item.ContentLocator;

/**
 * Component producing "raw" (streamed) NPM metadata from {@link MetadataStore} to be sent downstream for consumption
 * by NPM CLI or alike.
 */
public interface MetadataProducer
{
  ContentLocator produceRegistryRoot() throws IOException;

  @Nullable
  ContentLocator producePackageRoot(String packageName) throws IOException;

  @Nullable
  ContentLocator producePackageVersion(String packageName, String packageVersion) throws IOException;
}

package com.bolyuba.nexus.plugin.npm.metadata;

import java.io.IOException;

import org.sonatype.nexus.proxy.item.ContentLocator;

/**
 * Component consuming "raw" (streamed) NPM metadata from external source (either NPM CLI performing deploy or proxying
 * a NPM registry) and pushing it into {@link MetadataStore}.
 */
public interface MetadataConsumer
{
  int consumeRegistryRoot(final ContentLocator contentLocator) throws IOException;

  PackageRoot consumePackageRoot(final ContentLocator contentLocator) throws IOException;
}

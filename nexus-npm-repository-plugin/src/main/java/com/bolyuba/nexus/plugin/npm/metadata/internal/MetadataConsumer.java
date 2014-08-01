package com.bolyuba.nexus.plugin.npm.metadata.internal;

import java.io.IOException;

import org.sonatype.nexus.proxy.item.ContentLocator;

import com.bolyuba.nexus.plugin.npm.NpmRepository;
import com.bolyuba.nexus.plugin.npm.metadata.PackageRoot;
import com.bolyuba.nexus.plugin.npm.metadata.internal.MetadataParser.PackageRootIterator;

import static com.google.common.base.Preconditions.checkNotNull;

/**
 * Consumes "raw" (streamed) NPM metadata from external source (either NPM CLI performing deploy or proxying
 * a NPM registry) and pushing it into underlying store.
 */
public class MetadataConsumer
{
  private final NpmRepository npmRepository;

  private final MetadataParser metadataParser;

  private final MetadataStore metadataStore;

  public MetadataConsumer(final NpmRepository npmRepository,
                          final MetadataParser metadataParser,
                          final MetadataStore metadataStore)
  {
    this.npmRepository = checkNotNull(npmRepository);
    this.metadataParser = checkNotNull(metadataParser);
    this.metadataStore = checkNotNull(metadataStore);
  }

  public int consumeRegistryRoot(final ContentLocator contentLocator) throws IOException {
    try (final PackageRootIterator roots = metadataParser.parseRegistryRoot(contentLocator)) {
      return metadataStore.updatePackages(npmRepository, roots);
    }
  }

  public PackageRoot consumePackageRoot(final ContentLocator contentLocator) throws IOException {
    final PackageRoot packageRoot = metadataParser.parsePackageRoot(contentLocator);
    return metadataStore.updatePackage(npmRepository, packageRoot);
  }
}

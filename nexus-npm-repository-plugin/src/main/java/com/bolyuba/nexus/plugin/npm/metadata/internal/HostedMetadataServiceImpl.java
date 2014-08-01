package com.bolyuba.nexus.plugin.npm.metadata.internal;

import java.io.IOException;

import javax.annotation.Nullable;

import org.sonatype.nexus.proxy.item.ContentLocator;

import com.bolyuba.nexus.plugin.npm.metadata.HostedMetadataService;
import com.bolyuba.nexus.plugin.npm.metadata.PackageRoot;

import static com.google.common.base.Preconditions.checkNotNull;

/**
 * {@link HostedMetadataService} implementation.
 */
public class HostedMetadataServiceImpl
    implements HostedMetadataService
{
  private final MetadataConsumer metadataConsumer;

  private final MetadataProducer metadataProducer;

  public HostedMetadataServiceImpl(final MetadataConsumer metadataConsumer, final MetadataProducer metadataProducer) {
    this.metadataConsumer = checkNotNull(metadataConsumer);
    this.metadataProducer = checkNotNull(metadataProducer);
  }

  @Override
  public int consumeRegistryRoot(final ContentLocator contentLocator) throws IOException {
    return metadataConsumer.consumeRegistryRoot(contentLocator);
  }

  @Override
  public PackageRoot consumePackageRoot(final ContentLocator contentLocator) throws IOException {
    return metadataConsumer.consumePackageRoot(contentLocator);
  }

  @Override
  public ContentLocator produceRegistryRoot() throws IOException {
    return metadataProducer.produceRegistryRoot();
  }

  @Nullable
  @Override
  public ContentLocator producePackageRoot(final String packageName) throws IOException {
    return metadataProducer.producePackageRoot(packageName);
  }

  @Nullable
  @Override
  public ContentLocator producePackageVersion(final String packageName, final String packageVersion)
      throws IOException
  {
    return metadataProducer.producePackageVersion(packageName, packageVersion);
  }
}

package com.bolyuba.nexus.plugin.npm.metadata.internal;

import java.io.IOException;

import javax.annotation.Nullable;

import org.sonatype.nexus.proxy.item.ContentLocator;
import org.sonatype.sisu.goodies.common.ComponentSupport;

import com.bolyuba.nexus.plugin.npm.metadata.Generator;
import com.bolyuba.nexus.plugin.npm.metadata.Producer;
import com.bolyuba.nexus.plugin.npm.pkg.PackageRequest;

import static com.google.common.base.Preconditions.checkNotNull;

/**
 * Generator support.
 */
public abstract class GeneratorSupport
    extends ComponentSupport
    implements Generator
{
  private final Producer producer;

  public GeneratorSupport(final MetadataParser metadataParser) {
    this.producer = new GeneratorProducerImpl(this, checkNotNull(metadataParser));
  }

  public ContentLocator produceRegistryRoot(
      final PackageRequest request) throws IOException
  {
    return producer.produceRegistryRoot(request);
  }

  @Nullable
  public ContentLocator producePackageVersion(
      final PackageRequest request) throws IOException
  {
    return producer.producePackageVersion(request);
  }

  @Nullable
  public ContentLocator producePackageRoot(
      final PackageRequest request) throws IOException
  {
    return producer.producePackageRoot(request);
  }
}

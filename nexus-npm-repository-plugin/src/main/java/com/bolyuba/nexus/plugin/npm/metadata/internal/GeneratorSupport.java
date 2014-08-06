package com.bolyuba.nexus.plugin.npm.metadata.internal;

import java.io.IOException;

import javax.annotation.Nullable;

import org.sonatype.sisu.goodies.common.ComponentSupport;

import com.bolyuba.nexus.plugin.npm.metadata.Generator;
import com.bolyuba.nexus.plugin.npm.metadata.PackageRoot;
import com.bolyuba.nexus.plugin.npm.metadata.PackageVersion;
import com.bolyuba.nexus.plugin.npm.metadata.Producer;
import com.bolyuba.nexus.plugin.npm.pkg.PackageRequest;

import static com.google.common.base.Preconditions.checkArgument;

/**
 * {@link Generator} support class.
 */
public abstract class GeneratorSupport
    extends ComponentSupport
    implements Generator
{
  private final Producer producer;

  protected GeneratorSupport(final MetadataParser metadataParser) {
    this.producer = new GeneratorProducerImpl(this, metadataParser);
  }

  @Override
  public Producer getProducer() {
    return producer;
  }

  @Override
  public PackageRootIterator generateRegistryRoot(final PackageRequest request) throws IOException {
    return doGenerateRegistryRoot(request);
  }

  protected abstract PackageRootIterator doGenerateRegistryRoot(final PackageRequest request) throws IOException;

  @Nullable
  @Override
  public PackageRoot generatePackageRoot(final PackageRequest request) throws IOException {
    checkArgument(request.isPackageRoot(), "Package root request expected, but got %s",
        request.getPath());
    return doGeneratePackageRoot(request);
  }

  @Nullable
  protected abstract PackageRoot doGeneratePackageRoot(final PackageRequest request) throws IOException;

  @Nullable
  @Override
  public PackageVersion generatePackageVersion(final PackageRequest request) throws IOException {
    checkArgument(request.isPackageVersion(), "Package version request expected, but got %s",
        request.getPath());
    return doGeneratePackageVersion(request);
  }

  @Nullable
  protected abstract PackageVersion doGeneratePackageVersion(final PackageRequest request) throws IOException;
}

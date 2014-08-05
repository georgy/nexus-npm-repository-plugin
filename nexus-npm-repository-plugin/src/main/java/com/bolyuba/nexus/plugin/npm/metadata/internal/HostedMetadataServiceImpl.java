package com.bolyuba.nexus.plugin.npm.metadata.internal;

import java.io.IOException;

import javax.annotation.Nullable;

import org.sonatype.nexus.proxy.item.ContentLocator;

import com.bolyuba.nexus.plugin.npm.hosted.NpmHostedRepository;
import com.bolyuba.nexus.plugin.npm.metadata.HostedMetadataService;
import com.bolyuba.nexus.plugin.npm.metadata.PackageRoot;
import com.bolyuba.nexus.plugin.npm.metadata.PackageVersion;
import com.bolyuba.nexus.plugin.npm.metadata.Producer;
import com.bolyuba.nexus.plugin.npm.pkg.PackageRequest;

import static com.google.common.base.Preconditions.checkArgument;

/**
 * {@link HostedMetadataService} implementation.
 */
public class HostedMetadataServiceImpl
    extends GeneratorSupport
    implements HostedMetadataService
{
  private final NpmHostedRepository npmHostedRepository;

  private final MetadataGenerator metadataGenerator;

  private final MetadataParser metadataParser;

  public HostedMetadataServiceImpl(final NpmHostedRepository npmHostedRepository,
                                   final MetadataGenerator metadataGenerator,
                                   final MetadataParser metadataParser)
  {
    super(metadataParser);
    this.npmHostedRepository = npmHostedRepository;
    this.metadataGenerator = metadataGenerator;
    this.metadataParser = metadataParser;
  }


  @Override
  public PackageRoot consumePackageRoot(final PackageRequest request, final ContentLocator contentLocator)
      throws IOException
  {
    return metadataGenerator.consumePackageRoot(
        metadataParser.parsePackageRoot(npmHostedRepository.getId(), contentLocator));
  }

  @Override
  public PackageRootIterator generateRegistryRoot(final PackageRequest request) throws IOException {
    return metadataGenerator.generateRegistryRoot();
  }

  @Nullable
  @Override
  public PackageRoot generatePackageRoot(final PackageRequest request) throws IOException {
    checkArgument(request.isPackageRoot(), "Package root request expected, but got %s",
        request.getPath());
    return metadataGenerator.generatePackageRoot(request.getName());
  }

  @Nullable
  @Override
  public PackageVersion generatePackageVersion(final PackageRequest request) throws IOException {
    checkArgument(request.isPackageVersion(), "Package version request expected, but got %s",
        request.getPath());
    return metadataGenerator.generatePackageVersion(request.getName(),
        request.getVersion());
  }
}

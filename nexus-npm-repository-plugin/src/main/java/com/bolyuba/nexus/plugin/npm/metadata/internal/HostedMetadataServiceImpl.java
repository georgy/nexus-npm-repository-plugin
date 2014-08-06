package com.bolyuba.nexus.plugin.npm.metadata.internal;

import java.io.IOException;

import javax.annotation.Nullable;

import org.sonatype.nexus.proxy.item.ContentLocator;

import com.bolyuba.nexus.plugin.npm.hosted.NpmHostedRepository;
import com.bolyuba.nexus.plugin.npm.metadata.HostedMetadataService;
import com.bolyuba.nexus.plugin.npm.metadata.PackageRoot;
import com.bolyuba.nexus.plugin.npm.metadata.PackageVersion;
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
  public PackageRoot parsePackageRoot(final PackageRequest request, final ContentLocator contentLocator)
      throws IOException
  {
    checkArgument(request.isPackageRoot(), "Package root request expected, but got %s",
        request.getPath());
    final PackageRoot packageRoot = metadataParser.parsePackageRoot(npmHostedRepository.getId(), contentLocator);
    checkArgument(request.getName().equals(packageRoot.getName()),
        "Package root name '%s' and parsed content name '%s' mismatch", request.getName(), packageRoot.getName());
    checkArgument(!packageRoot.isIncomplete(), "Incomplete package root parsed");
    return packageRoot;
  }

  @Override
  public PackageRoot consumePackageRoot(final PackageRequest request, final PackageRoot packageRoot)
      throws IOException
  {
    return metadataGenerator.consumePackageRoot(packageRoot);
  }

  @Override
  protected PackageRootIterator doGenerateRegistryRoot(final PackageRequest request) throws IOException {
    return metadataGenerator.generateRegistryRoot();
  }

  @Nullable
  @Override
  protected PackageRoot doGeneratePackageRoot(final PackageRequest request) throws IOException {
    return metadataGenerator.generatePackageRoot(request.getName());
  }

  @Nullable
  @Override
  protected PackageVersion doGeneratePackageVersion(final PackageRequest request) throws IOException {
    return metadataGenerator.generatePackageVersion(request.getName(),
        request.getVersion());
  }
}

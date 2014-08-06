package com.bolyuba.nexus.plugin.npm.metadata.internal;

import java.io.IOException;

import javax.annotation.Nullable;

import com.bolyuba.nexus.plugin.npm.group.NpmGroupRepository;
import com.bolyuba.nexus.plugin.npm.metadata.GroupMetadataService;
import com.bolyuba.nexus.plugin.npm.metadata.PackageRoot;
import com.bolyuba.nexus.plugin.npm.metadata.PackageVersion;
import com.bolyuba.nexus.plugin.npm.pkg.PackageRequest;

/**
 * {@link GroupMetadataService} implementation.
 */
public class GroupMetadataServiceImpl
    extends GeneratorSupport
    implements GroupMetadataService
{
  private final NpmGroupRepository npmGroupRepository;

  private final MetadataGenerator metadataGenerator;

  private final MetadataParser metadataParser;

  public GroupMetadataServiceImpl(final NpmGroupRepository npmGroupRepository,
                                  final MetadataGenerator metadataGenerator,
                                  final MetadataParser metadataParser)
  {
    super(metadataParser);
    this.npmGroupRepository = npmGroupRepository;
    this.metadataGenerator = metadataGenerator;
    this.metadataParser = metadataParser;
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

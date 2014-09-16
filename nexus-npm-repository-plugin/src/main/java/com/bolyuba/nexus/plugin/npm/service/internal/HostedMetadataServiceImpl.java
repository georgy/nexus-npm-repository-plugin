package com.bolyuba.nexus.plugin.npm.service.internal;

import java.io.IOException;

import javax.annotation.Nullable;

import org.sonatype.nexus.proxy.item.ContentLocator;

import com.bolyuba.nexus.plugin.npm.hosted.NpmHostedRepository;
import com.bolyuba.nexus.plugin.npm.service.HostedMetadataService;
import com.bolyuba.nexus.plugin.npm.service.PackageRequest;
import com.bolyuba.nexus.plugin.npm.service.PackageRoot;

import static com.google.common.base.Preconditions.checkArgument;

/**
 * {@link HostedMetadataService} implementation.
 */
public class HostedMetadataServiceImpl
    extends GeneratorWithStoreSupport<NpmHostedRepository>
    implements HostedMetadataService
{
  public HostedMetadataServiceImpl(final NpmHostedRepository npmHostedRepository,
                                   final MetadataStore metadataStore,
                                   final MetadataParser metadataParser)
  {
    super(npmHostedRepository, metadataParser, metadataStore);
  }

  @Override
  public PackageRoot parsePackageRoot(final PackageRequest request, final ContentLocator contentLocator)
      throws IOException
  {
    checkArgument(request.isPackageRoot(), "Package root request expected, but got %s",
        request.getPath());
    final PackageRoot packageRoot = metadataParser.parsePackageRoot(getNpmRepository().getId(), contentLocator);
    checkArgument(request.getName().equals(packageRoot.getName()),
        "Package root name '%s' and parsed content name '%s' mismatch", request.getName(), packageRoot.getName());
    checkArgument(!packageRoot.isIncomplete(), "Incomplete package root parsed");
    return packageRoot;
  }

  @Override
  public PackageRoot consumePackageRoot(final PackageRoot packageRoot)
      throws IOException
  {
    return metadataStore.updatePackage(getNpmRepository(), packageRoot);
  }

  @Nullable
  @Override
  protected PackageRoot doGeneratePackageRoot(final PackageRequest request) throws IOException {
    return metadataStore.getPackageByName(getNpmRepository(), request.getName());
  }
}

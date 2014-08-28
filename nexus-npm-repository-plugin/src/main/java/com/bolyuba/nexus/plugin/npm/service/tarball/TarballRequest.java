package com.bolyuba.nexus.plugin.npm.service.tarball;

import org.sonatype.nexus.proxy.ResourceStoreRequest;

import com.bolyuba.nexus.plugin.npm.service.PackageRoot;
import com.bolyuba.nexus.plugin.npm.service.PackageVersion;

import static com.google.common.base.Preconditions.checkArgument;
import static com.google.common.base.Preconditions.checkNotNull;

/**
 * Class encapsulating a NPM Tarball request.
 */
public class TarballRequest
{
  private final ResourceStoreRequest resourceStoreRequest;

  private final PackageRoot packageRoot;

  private final PackageVersion packageVersion;

  public TarballRequest(final ResourceStoreRequest resourceStoreRequest, final PackageRoot packageRoot,
                        final PackageVersion packageVersion)
  {
    checkArgument(!packageRoot.isIncomplete(), "Incomplete package %s tarball cannot be requested");
    this.resourceStoreRequest = checkNotNull(resourceStoreRequest);
    this.packageRoot = checkNotNull(packageRoot);
    this.packageVersion = checkNotNull(packageVersion);
  }

  public ResourceStoreRequest getResourceStoreRequest() {
    return resourceStoreRequest;
  }

  public PackageRoot getPackageRoot() {
    return packageRoot;
  }

  public PackageVersion getPackageVersion() {
    return packageVersion;
  }
}

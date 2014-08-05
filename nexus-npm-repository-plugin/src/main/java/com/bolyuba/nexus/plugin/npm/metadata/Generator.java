package com.bolyuba.nexus.plugin.npm.metadata;

import java.io.IOException;

import javax.annotation.Nullable;

import com.bolyuba.nexus.plugin.npm.metadata.internal.PackageRootIterator;
import com.bolyuba.nexus.plugin.npm.pkg.PackageRequest;

/**
 * Metadata produces that generates entities to be sent downstream to clint.
 */
public interface Generator
{
  PackageRootIterator generateRegistryRoot(PackageRequest request) throws IOException;

  @Nullable
  PackageRoot generatePackageRoot(PackageRequest request) throws IOException;

  @Nullable
  PackageVersion generatePackageVersion(PackageRequest request) throws IOException;
}


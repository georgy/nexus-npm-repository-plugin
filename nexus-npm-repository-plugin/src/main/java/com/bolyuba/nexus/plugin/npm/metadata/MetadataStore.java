package com.bolyuba.nexus.plugin.npm.metadata;

import java.util.Iterator;
import java.util.List;

import com.bolyuba.nexus.plugin.npm.NpmRepository;

public interface MetadataStore
{
  List<String> listPackageNames(NpmRepository repository);

  PackageRoot getPackageByName(NpmRepository repository, String packageName);

  PackageRoot updatePackage(NpmRepository repository, PackageRoot packageRoot);

  int updatePackages(NpmRepository repository, Iterator<PackageRoot> packageRootIterator);
}

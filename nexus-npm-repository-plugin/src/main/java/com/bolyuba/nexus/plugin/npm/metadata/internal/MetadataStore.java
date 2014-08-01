package com.bolyuba.nexus.plugin.npm.metadata.internal;

import java.util.Iterator;
import java.util.List;

import com.bolyuba.nexus.plugin.npm.NpmRepository;
import com.bolyuba.nexus.plugin.npm.metadata.PackageRoot;

/**
 * Database.
 */
public interface MetadataStore
{
  /**
   * Returns the names of present packages in the store.
   */
  List<String> listPackageNames(NpmRepository repository);

  /**
   * Retrieves a package from store by name.
   */
  PackageRoot getPackageByName(NpmRepository repository, String packageName);

  /**
   * Deletes package from store, returns {@code true} if package existed.
   */
  boolean deletePackageByName(NpmRepository repository, String packageName);

  /**
   * Updates one single package, merging it if necessary.
   *
   * @see PackageRoot#overlay(PackageRoot)
   */
  PackageRoot updatePackage(NpmRepository repository, PackageRoot packageRoot);

  /**
   * Massive update of packages, merging each of them if necessary.
   *
   * @see PackageRoot#overlay(PackageRoot)
   */
  int updatePackages(NpmRepository repository, Iterator<PackageRoot> packageRootIterator);
}

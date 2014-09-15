package com.bolyuba.nexus.plugin.npm.service.internal;

import java.io.IOException;
import java.util.Iterator;
import java.util.List;

import javax.annotation.Nullable;

import org.sonatype.nexus.web.BaseUrlHolder;
import org.sonatype.sisu.goodies.common.SimpleFormat;

import com.bolyuba.nexus.plugin.npm.NpmRepository;
import com.bolyuba.nexus.plugin.npm.service.PackageRoot;
import com.bolyuba.nexus.plugin.npm.service.PackageVersion;

import static com.google.common.base.Preconditions.checkNotNull;

/**
 * Produces NPM metadata from underlying store, filtering it and applying other logic. Packages should be sent
 * downstream only by getting the over this component, as underlying store stores JSON documents as-is (as fetched from
 * remote)!
 */
public class MetadataGenerator
{
  private final NpmRepository npmRepository;

  private final MetadataStore metadataStore;

  public MetadataGenerator(final NpmRepository npmRepository, final MetadataStore metadataStore) {
    this.npmRepository = checkNotNull(npmRepository);
    this.metadataStore = checkNotNull(metadataStore);
  }

  public PackageRoot consumePackageRoot(final PackageRoot packageRoot)
      throws IOException
  {
    return metadataStore.updatePackage(npmRepository, packageRoot);
  }

  public PackageRootIterator generateRegistryRoot() {
    return new MetadataStorePackageRootIterator(metadataStore.listPackageNames(npmRepository));
  }

  @Nullable
  public PackageRoot generatePackageRoot(final String packageName) {
    final PackageRoot root = metadataStore.getPackageByName(npmRepository, packageName);
    return generatePackageRoot(root);
  }

  @Nullable
  public PackageRoot generatePackageRoot(final PackageRoot root) {
    if (root == null || root.isIncomplete()) {
      return null;
    }
    filterPackageRoot(root);
    return root;
  }

  @Nullable
  public PackageVersion generatePackageVersion(final PackageRoot root, final String packageVersion) {
    if (root == null || root.isUnpublished()) {
      return null;
    }
    final PackageVersion version = root.getVersions().get(packageVersion);
    if (version == null || version.isIncomplete()) {
      return null;
    }
    filterPackageVersion(root, version);
    return version;
  }

  // ==

  private void filterPackageRoot(final PackageRoot packageRoot) {
    packageRoot.getRaw().remove("_id"); // TODO: why? Original code did this too
    packageRoot.getRaw().remove("_rev"); // TODO: why? Original code did this too
    for (PackageVersion packageVersion : packageRoot.getVersions().values()) {
      filterPackageVersion(packageRoot, packageVersion);
    }
  }

  private void filterPackageVersion(final PackageRoot packageRoot, final PackageVersion packageVersion) {
    packageVersion.setDistTarball(SimpleFormat
        .format("%s/content/repositories/%s/%s/-/%s", BaseUrlHolder.get(), npmRepository.getId(),
            packageVersion.getName(), packageVersion.getDistTarballFilename()));
    packageVersion.getRaw().remove("_id"); // TODO: why? Original code did this too
    packageVersion.getRaw().remove("_rev"); // TODO: why? Original code did this too
    final String versionTarballShasum = PackageVersion.createShasumVersionKey(packageVersion.getVersion());
    if (packageRoot.getProperties().containsKey(versionTarballShasum)) {
      // this publishes proper SHA1 for ALL packages already proxies by NX
      packageVersion.setDistShasum(packageRoot.getProperties().get(versionTarballShasum));
    }
  }

  // ==

  private class MetadataStorePackageRootIterator
      implements PackageRootIterator
  {
    private final Iterator<String> packageNames;

    private MetadataStorePackageRootIterator(final List<String> packageNames) {
      this.packageNames = packageNames.iterator();
    }

    @Override
    public void close() throws IOException {
      // nop
    }

    @Override
    public boolean hasNext() {
      return packageNames.hasNext();
    }

    @Override
    public PackageRoot next() {
      final PackageRoot root = metadataStore.getPackageByName(npmRepository, packageNames.next());
      if (root == null) {
        return null; // TODO: This is error actually, as iterator should not return null
      }
      if (!root.isUnpublished()) {
        root.shrinkPackageVersions();
      }
      filterPackageRoot(root);
      return root;
    }

    @Override
    public void remove() {
      throw new UnsupportedOperationException("Remove not supported.");
    }
  }
}

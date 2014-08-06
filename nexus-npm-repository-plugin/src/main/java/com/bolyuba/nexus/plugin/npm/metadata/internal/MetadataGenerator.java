package com.bolyuba.nexus.plugin.npm.metadata.internal;

import java.io.IOException;
import java.util.Iterator;
import java.util.List;

import javax.annotation.Nullable;

import org.sonatype.nexus.web.BaseUrlHolder;
import org.sonatype.sisu.goodies.common.SimpleFormat;

import com.bolyuba.nexus.plugin.npm.NpmRepository;
import com.bolyuba.nexus.plugin.npm.metadata.PackageRoot;
import com.bolyuba.nexus.plugin.npm.metadata.PackageVersion;
import com.google.common.base.Throwables;

import static com.google.common.base.Preconditions.checkNotNull;

/**
 * Produces NPM metadata from underlying store, filtering it and applying other logic. Packages should be sent downstream
 * only by getting the over this component, as underlying store stores JSON documents as-is (as fetched from remote)!
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
  public PackageRoot generateShrinkedPackageRoot(final String packageName) throws IOException {
    final PackageRoot root = metadataStore.getPackageByName(npmRepository, packageName);
    if (root == null) {
      return null;
    }
    if (!root.isIncomplete()) {
      root.shrinkToLatestVersionOnly();
    }
    filterPackageRoot(root);
    return root;
  }

  @Nullable
  public PackageRoot generatePackageRoot(final String packageName) throws IOException {
    final PackageRoot root = metadataStore.getPackageByName(npmRepository, packageName);
    if (root == null || root.isIncomplete()) {
      return null;
    }
    filterPackageRoot(root);
    return root;
  }

  @Nullable
  public PackageVersion generatePackageVersion(final String packageName, final String packageVersion)
      throws IOException
  {
    final PackageRoot root = metadataStore.getPackageByName(npmRepository, packageName);
    if (root == null || root.isUnpublished()) {
      return null;
    }
    final PackageVersion version = root.getVersions().get(packageVersion);
    if (version == null || version.isIncomplete()) {
      return null;
    }
    filterPackageVersion(version);
    return version;
  }

  // ==

  private void filterPackageRoot(final PackageRoot packageRoot) {
    packageRoot.getRaw().remove("_id"); // TODO: why? Original code did this too
    packageRoot.getRaw().remove("_rev"); // TODO: why? Original code did this too
    for (PackageVersion packageVersion : packageRoot.getVersions().values()) {
      filterPackageVersion(packageVersion);
    }
  }

  private void filterPackageVersion(final PackageVersion packageVersion) {
    packageVersion.setDistTarball(SimpleFormat
        .format("%s/content/repositories/%s/%s/-/%s-%s.tgz", BaseUrlHolder.get(), npmRepository.getId(),
            packageVersion.getName(), packageVersion.getName(),
            packageVersion.getVersion()));
    packageVersion.getRaw().remove("_id"); // TODO: why? Original code did this too
    packageVersion.getRaw().remove("_rev"); // TODO: why? Original code did this too
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
      try {
        return generateShrinkedPackageRoot(packageNames.next());
      }
      catch (IOException e) {
        throw Throwables.propagate(e);
      }
    }

    @Override
    public void remove() {
      throw new UnsupportedOperationException("Remove not supported.");
    }
  }
}

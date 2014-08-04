package com.bolyuba.nexus.plugin.npm.metadata.internal;

import java.io.IOException;
import java.util.List;

import javax.annotation.Nullable;

import org.sonatype.nexus.web.BaseUrlHolder;
import org.sonatype.sisu.goodies.common.SimpleFormat;

import com.bolyuba.nexus.plugin.npm.NpmRepository;
import com.bolyuba.nexus.plugin.npm.metadata.PackageRoot;
import com.bolyuba.nexus.plugin.npm.metadata.PackageVersion;

import static com.google.common.base.Preconditions.checkNotNull;

/**
 * Produces NPM metadata from underlying store, filtering it and applying other logic.
 */
public class MetadataGenerator
{
  private final NpmRepository npmRepository;

  private final MetadataStore metadataStore;

  public MetadataGenerator(final NpmRepository npmRepository, final MetadataStore metadataStore) {
    this.npmRepository = checkNotNull(npmRepository);
    this.metadataStore = checkNotNull(metadataStore);
  }

  public List<String> listPackageNames() {
    return metadataStore.listPackageNames(npmRepository);
  }

  @Nullable
  public PackageRoot generateShrinkedPackageRoot(final String packageName) throws IOException {
    final PackageRoot root = metadataStore.getPackageByName(npmRepository, packageName);
    if (root == null) {
      return null;
    }
    root.shrinkToLatestVersionOnly();
    return root;
  }

  @Nullable
  public PackageRoot generatePackageRoot(final String packageName) throws IOException {
    final PackageRoot root = metadataStore.getPackageByName(npmRepository, packageName);
    if (root == null || root.isIncomplete()) {
      return null;
    }
    for (PackageVersion packageVersion : root.getVersions().values()) {
      filterPackageVersion(packageVersion);
    }
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

  private void filterPackageVersion(final PackageVersion packageVersion) {
    packageVersion.setDistTarball(SimpleFormat
        .format("%s/content/repositories/%s/%s/-/%s-%s.tgz", BaseUrlHolder.get(), npmRepository.getId(),
            packageVersion.getName(), packageVersion.getName(),
            packageVersion.getVersion()));
  }
}

package com.bolyuba.nexus.plugin.npm.metadata.internal;

import java.io.IOException;

import javax.annotation.Nullable;

import org.sonatype.nexus.proxy.item.StringContentLocator;

import com.bolyuba.nexus.plugin.npm.NpmRepository;
import com.bolyuba.nexus.plugin.npm.metadata.MetadataProducer;
import com.bolyuba.nexus.plugin.npm.metadata.MetadataStore;
import com.bolyuba.nexus.plugin.npm.metadata.PackageRoot;
import com.bolyuba.nexus.plugin.npm.metadata.PackageVersion;
import com.fasterxml.jackson.databind.ObjectMapper;

import static com.google.common.base.Preconditions.checkNotNull;

/**
 * Created by cstamas on 29/07/14.
 */
public class MetadataProducerImpl
    implements MetadataProducer
{
  private final NpmRepository npmRepository;

  private final MetadataStore metadataStore;

  private final ObjectMapper objectMapper;

  public MetadataProducerImpl(final NpmRepository npmRepository, final MetadataStore metadataStore) {
    this.npmRepository = checkNotNull(npmRepository);
    this.metadataStore = checkNotNull(metadataStore);
    this.objectMapper = new ObjectMapper(); // this generates registry JSON
  }

  @Override
  public RegistryRootContentLocator produceRegistryRoot() throws IOException {

    return new RegistryRootContentLocator(this, metadataStore.listPackageNames(npmRepository));
  }

  @Nullable
  public StringContentLocator produceShrinkedPackageRoot(final String packageName) throws IOException {
    final PackageRoot root = metadataStore.getPackageByName(npmRepository, packageName);
    if (root == null) {
      return null;
    }
    root.shrinkToLatestVersionOnly();
    final String jsonString = objectMapper.writeValueAsString(root.getRaw());
    return new StringContentLocator(jsonString, NpmRepository.JSON_MIME_TYPE);
  }

  @Nullable
  @Override
  public StringContentLocator producePackageRoot(final String packageName) throws IOException {
    final PackageRoot root = metadataStore.getPackageByName(npmRepository, packageName);
    if (root == null || root.isIncomplete()) {
      return null;
    }
    for (PackageVersion packageVersion : root.getVersions().values()) {
      filterPackageVersion(packageVersion);
    }
    final String jsonString = objectMapper.writeValueAsString(root.getRaw());
    return new StringContentLocator(jsonString, NpmRepository.JSON_MIME_TYPE);
  }

  @Nullable
  @Override
  public StringContentLocator producePackageVersion(final String packageName, final String packageVersion)
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
    final String jsonString = objectMapper.writeValueAsString(version.getRaw());
    return new StringContentLocator(jsonString, NpmRepository.JSON_MIME_TYPE);
  }

  // ==

  private void filterPackageVersion(final PackageVersion packageVersion) {
    // TODO: set tarball url
    packageVersion.setDistTarball("boo");
  }
}

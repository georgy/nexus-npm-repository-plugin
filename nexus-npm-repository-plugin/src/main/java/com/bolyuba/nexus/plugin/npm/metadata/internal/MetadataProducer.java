package com.bolyuba.nexus.plugin.npm.metadata.internal;

import java.io.IOException;
import java.io.InputStream;
import java.util.Iterator;
import java.util.List;

import javax.annotation.Nullable;

import org.sonatype.nexus.proxy.item.AbstractContentLocator;
import org.sonatype.nexus.proxy.item.ContentLocator;
import org.sonatype.nexus.proxy.item.StringContentLocator;

import com.bolyuba.nexus.plugin.npm.NpmRepository;
import com.bolyuba.nexus.plugin.npm.metadata.PackageRoot;
import com.bolyuba.nexus.plugin.npm.metadata.PackageVersion;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.google.common.base.Charsets;
import com.google.common.base.Throwables;
import com.google.common.collect.Lists;
import com.google.common.io.ByteSource;

import static com.google.common.base.Preconditions.checkNotNull;

/**
 * Produces "raw" (streamed) NPM metadata from underlying store to be sent downstream for consumption
 * by NPM CLI or alike.
 */
public class MetadataProducer
{
  private final MetadataGenerator metadataGenerator;

  private final ObjectMapper objectMapper;

  public MetadataProducer(final MetadataGenerator metadataGenerator) {
    this.metadataGenerator = checkNotNull(metadataGenerator);
    this.objectMapper = new ObjectMapper(); // this generates registry JSON
  }

  public RegistryRootContentLocator produceRegistryRoot() throws IOException {
    return new RegistryRootContentLocator(this, metadataGenerator.listPackageNames());
  }

  @Nullable
  public StringContentLocator produceShrinkedPackageRoot(final String packageName) throws IOException {
    final PackageRoot root = metadataGenerator.generateShrinkedPackageRoot(packageName);
    if (root == null) {
      return null;
    }
    final String jsonString = objectMapper.writeValueAsString(root.getRaw());
    return new StringContentLocator(jsonString, NpmRepository.JSON_MIME_TYPE);
  }

  @Nullable
  public StringContentLocator producePackageRoot(final String packageName) throws IOException {
    final PackageRoot root = metadataGenerator.generatePackageRoot(packageName);
    if (root == null) {
      return null;
    }
    final String jsonString = objectMapper.writeValueAsString(root.getRaw());
    return new StringContentLocator(jsonString, NpmRepository.JSON_MIME_TYPE);
  }

  @Nullable
  public StringContentLocator producePackageVersion(final String packageName, final String packageVersion)
      throws IOException
  {
    final PackageVersion version = metadataGenerator.generatePackageVersion(packageName, packageVersion);
    if (version == null) {
      return null;
    }
    final String jsonString = objectMapper.writeValueAsString(version.getRaw());
    return new StringContentLocator(jsonString, NpmRepository.JSON_MIME_TYPE);
  }

  /**
   * A content locator that streams the potentially huge registry root JSON document out of all of the package
   * documents.
   */
  private static class RegistryRootContentLocator
      extends AbstractContentLocator
      implements Iterator<ByteSource>
  {
    private final MetadataProducer metadataProducer;

    private final Iterator<String> packageNameIterator;

    private boolean first;

    protected RegistryRootContentLocator(final MetadataProducer metadataProducer, final List<String> packageNames) {
      super(NpmRepository.JSON_MIME_TYPE, false, ContentLocator.UNKNOWN_LENGTH);
      this.metadataProducer = checkNotNull(metadataProducer);
      this.packageNameIterator = checkNotNull(packageNames).iterator();
      this.first = true;
    }

    @Override
    public InputStream getContent() throws IOException {
      return ByteSource.concat(this).openStream();
    }

    // ==

    @Override
    public boolean hasNext() {
      return first || packageNameIterator.hasNext();
    }

    @Override
    public ByteSource next() {
      try {
        final List<ByteSource> sources = Lists.newArrayList();
        if (first) {
          first = false;
          sources.add(ByteSource.wrap("{".getBytes(Charsets.UTF_8)));
        }
        if (packageNameIterator.hasNext()) {
          final String packageName = packageNameIterator.next();
          sources.add(ByteSource.wrap(("\"" + packageName + "\":").getBytes(Charsets.UTF_8)));
          sources.add(ByteSource.wrap(metadataProducer.produceShrinkedPackageRoot(packageName).getByteArray()));
        }
        if (!packageNameIterator.hasNext()) {
          sources.add(ByteSource.wrap("}".getBytes(Charsets.UTF_8)));
        }
        else {
          sources.add(ByteSource.wrap(",".getBytes(Charsets.UTF_8)));
        }
        return ByteSource.concat(sources);
      }
      catch (Exception e) {
        throw Throwables.propagate(e);
      }
    }

    @Override
    public void remove() {
      throw new UnsupportedOperationException("Remove not supported");
    }
  }
}

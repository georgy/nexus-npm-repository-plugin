package com.bolyuba.nexus.plugin.npm.metadata.internal;

import java.io.IOException;
import java.util.Iterator;
import java.util.Map;

import org.sonatype.nexus.proxy.item.ContentLocator;

import com.bolyuba.nexus.plugin.npm.NpmRepository;
import com.bolyuba.nexus.plugin.npm.metadata.MetadataConsumer;
import com.bolyuba.nexus.plugin.npm.metadata.MetadataStore;
import com.bolyuba.nexus.plugin.npm.metadata.PackageRoot;
import com.fasterxml.jackson.core.JsonParser;
import com.fasterxml.jackson.core.JsonToken;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.google.common.base.Throwables;

import static com.google.common.base.Preconditions.checkArgument;
import static com.google.common.base.Preconditions.checkNotNull;

/**
 * Created by cstamas on 29/07/14.
 */
public class MetadataConsumerImpl
    implements MetadataConsumer
{
  private final NpmRepository npmRepository;

  private final MetadataStore metadataStore;

  private final ObjectMapper objectMapper;

  public MetadataConsumerImpl(final NpmRepository npmRepository, final MetadataStore metadataStore) {
    this.npmRepository = checkNotNull(npmRepository);
    this.metadataStore = checkNotNull(metadataStore);
    this.objectMapper = new ObjectMapper(); // this parses registry JSON
  }

  @Override
  public int consumeRegistryRoot(final ContentLocator contentLocator) throws IOException {
    checkArgument(NpmRepository.JSON_MIME_TYPE.equals(contentLocator.getMimeType()), "JSON is expected inout!");
    try (final JsonParser parser = objectMapper.getFactory().createParser(contentLocator.getContent())) {
      return metadataStore.updatePackages(npmRepository, new PackageRootIterator(parser));
    }
  }

  @Override
  public PackageRoot consumePackageRoot(final ContentLocator contentLocator) throws IOException {
    checkArgument(NpmRepository.JSON_MIME_TYPE.equals(contentLocator.getMimeType()), "JSON is expected inout!");
    try (final JsonParser parser = objectMapper.getFactory().createParser(contentLocator.getContent())) {
      final PackageRoot packageRoot = parsePackageRoot(parser);
      checkArgument(!packageRoot.isIncomplete(),
          "Wrong API use, incomplete package roots should not be consumed this way!");
      return metadataStore.updatePackage(npmRepository, packageRoot);
    }
  }

  // ==

  private PackageRoot parsePackageRoot(final JsonParser parser) {
    try {
      final Map<String, Object> raw = objectMapper.readValue(parser, new TypeReference<Map<String, Object>>() {});
      raw.remove("_attachments"); // TODO: on NPM deploy this is where tarball is base64 encoded, so best would be
      // to tell Jackson to completely ignore _attachments. But how?
      return new PackageRoot(npmRepository.getId(), raw);
    }
    catch (Exception e) {
      throw Throwables.propagate(e);
    }
  }

  // ==

  private class PackageRootIterator
      implements Iterator<PackageRoot>
  {
    private final JsonParser parser;

    private PackageRoot nextPackageRoot;

    private PackageRootIterator(final JsonParser parser) {
      this.parser = parser;
      nextPackageRoot = getNext();
    }

    @Override
    public boolean hasNext() {
      return nextPackageRoot != null;
    }

    @Override
    public PackageRoot next() {
      final PackageRoot next = nextPackageRoot;
      nextPackageRoot = getNext();
      return next;
    }

    private PackageRoot getNext() {
      try {
        while (parser.nextToken() != null) {
          if (parser.getCurrentToken() != JsonToken.FIELD_NAME) {
            continue;
          }
          // we are at field name, skip any field starting with underscores
          if (parser.getCurrentName().startsWith("_")) {
            continue; // skip it
          }
          parser.nextValue();
          return parsePackageRoot(parser);
        }
        return null;
      }
      catch (Exception e) {
        throw Throwables.propagate(e);
      }
    }

    @Override
    public void remove() {
      throw new UnsupportedOperationException("Remove unsupported");
    }
  }
}

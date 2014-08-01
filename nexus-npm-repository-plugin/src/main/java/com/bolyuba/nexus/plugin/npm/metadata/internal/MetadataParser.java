package com.bolyuba.nexus.plugin.npm.metadata.internal;

import java.io.Closeable;
import java.io.IOException;
import java.util.Iterator;
import java.util.Map;

import org.sonatype.nexus.proxy.item.ContentLocator;

import com.bolyuba.nexus.plugin.npm.NpmRepository;
import com.bolyuba.nexus.plugin.npm.metadata.PackageRoot;
import com.fasterxml.jackson.core.JsonParser;
import com.fasterxml.jackson.core.JsonToken;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.google.common.base.Throwables;

import static com.google.common.base.Preconditions.checkArgument;
import static com.google.common.base.Preconditions.checkNotNull;

/**
 * Parses "raw" (streamed or not) NPM metadata from external source (either NPM CLI performing deploy or
 * proxying a NPM registry) and producing {@link PackageRoot} instances.
 */
public class MetadataParser
{
  public static interface PackageRootIterator
      extends Iterator<PackageRoot>, Closeable
  {
  }

  private final NpmRepository npmRepository;

  private final ObjectMapper objectMapper;

  public MetadataParser(final NpmRepository npmRepository) {
    this.npmRepository = checkNotNull(npmRepository);
    this.objectMapper = new ObjectMapper(); // this parses registry JSON
  }

  public PackageRootIterator parseRegistryRoot(final ContentLocator contentLocator) throws IOException {
    checkArgument(NpmRepository.JSON_MIME_TYPE.equals(contentLocator.getMimeType()), "JSON is expected inout!");
    return new PackageRootIteratorImpl(objectMapper.getFactory().createParser(contentLocator.getContent()));
  }

  public PackageRoot parsePackageRoot(final ContentLocator contentLocator) throws IOException {
    checkArgument(NpmRepository.JSON_MIME_TYPE.equals(contentLocator.getMimeType()), "JSON is expected inout!");
    try (final JsonParser parser = objectMapper.getFactory().createParser(contentLocator.getContent())) {
      final PackageRoot packageRoot = parsePackageRoot(parser);
      checkArgument(!packageRoot.isIncomplete(),
          "Wrong API use, incomplete package roots should not be consumed this way!");
      return packageRoot;
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

  private class PackageRootIteratorImpl
      implements PackageRootIterator
  {
    private final JsonParser parser;

    private PackageRoot nextPackageRoot;

    private PackageRootIteratorImpl(final JsonParser parser) {
      this.parser = parser;
      nextPackageRoot = getNext();
    }

    @Override
    public void close() throws IOException {
      parser.close();
    }

    @Override
    public boolean hasNext() {
      final boolean hasNext = nextPackageRoot != null;
      if (!hasNext) {
        try {
          close();
        }
        catch (IOException e) {
          throw Throwables.propagate(e);
        }
      }
      return hasNext;
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

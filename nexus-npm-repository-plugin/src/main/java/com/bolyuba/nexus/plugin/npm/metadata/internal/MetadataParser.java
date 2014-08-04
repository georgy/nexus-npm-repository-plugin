package com.bolyuba.nexus.plugin.npm.metadata.internal;

import java.io.Closeable;
import java.io.File;
import java.io.IOException;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import org.sonatype.nexus.proxy.item.ContentLocator;

import com.bolyuba.nexus.plugin.npm.NpmRepository;
import com.bolyuba.nexus.plugin.npm.metadata.PackageAttachment;
import com.bolyuba.nexus.plugin.npm.metadata.PackageRoot;
import com.fasterxml.jackson.core.JsonParser;
import com.fasterxml.jackson.core.JsonToken;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.google.common.base.Throwables;
import com.google.common.collect.Maps;
import com.google.common.io.Files;

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

  private final File temporaryDirectory;

  private final NpmRepository npmRepository;

  private final ObjectMapper objectMapper;

  public MetadataParser(final File temporaryDirectory, final NpmRepository npmRepository) {
    this.temporaryDirectory = checkNotNull(temporaryDirectory);
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

  private PackageRoot parsePackageRoot(final JsonParser parser) throws IOException {
    final Map<String, Object> raw = Maps.newHashMap();
    final Map<String, PackageAttachment> attachments = Maps.newHashMap();
    checkArgument(parser.nextToken() == JsonToken.START_OBJECT, "Unexpected input %s, expected %s",
        parser.getCurrentToken(), JsonToken.START_OBJECT);
    while (parser.nextToken() == JsonToken.FIELD_NAME) {
      final String fieldName = parser.getCurrentName();
      if ("_attachments".equals(fieldName)) {
        parsePackageAttachments(parser, attachments);
        continue;
      }
      final JsonToken token = parser.nextValue();
      if (token == JsonToken.START_OBJECT) {
        raw.put(fieldName, parser.readValueAs(new TypeReference<Map<String, Object>>() {}));
      }
      else if (token == JsonToken.START_ARRAY) {
        raw.put(fieldName, parser.readValueAs(new TypeReference<List<Object>>() {}));
      }
      else {
        switch (token) {
          case VALUE_NULL: {
            raw.put(fieldName, null);
            break;
          }
          case VALUE_FALSE: {
            raw.put(fieldName, Boolean.FALSE);
            break;
          }
          case VALUE_TRUE: {
            raw.put(fieldName, Boolean.TRUE);
            break;
          }
          case VALUE_NUMBER_INT: {
            raw.put(fieldName, parser.getValueAsInt());
            break;
          }
          case VALUE_NUMBER_FLOAT: {
            raw.put(fieldName, parser.getValueAsDouble());
            break;
          }
          case VALUE_STRING: {
            raw.put(fieldName, parser.getValueAsString());
            break;
          }
          default: {
            throw new IllegalArgumentException("Unexpected token: " + token);
          }
        }
        raw.put(fieldName, parser.getValueAsString());
      }
    }
    final PackageRoot result = new PackageRoot(npmRepository.getId(), raw);
    if (!attachments.isEmpty()) {
      result.getAttachments().putAll(attachments);
    }
    return result;
  }

  private void parsePackageAttachments(final JsonParser parser,
                                       final Map<String, PackageAttachment> attachments) throws IOException
  {
    checkArgument(parser.nextToken() == JsonToken.START_OBJECT, "Unexpected input %s, expected %s",
        parser.getCurrentToken(), JsonToken.START_OBJECT);
    while (parser.nextToken() == JsonToken.FIELD_NAME) {
      final PackageAttachment attachment = parsePackageAttachment(parser);
      attachments.put(attachment.getName(), attachment);
    }
  }

  private PackageAttachment parsePackageAttachment(final JsonParser parser) throws IOException {
    String name = parser.getCurrentName();
    String contentType = "application/octet-stream";
    long length = ContentLocator.UNKNOWN_LENGTH;
    File file = null;
    checkArgument(parser.nextToken() == JsonToken.START_OBJECT, "Unexpected input %s, expected %s",
        parser.getCurrentToken(), JsonToken.START_OBJECT);
    while (parser.nextToken() == JsonToken.FIELD_NAME) {
      final String fieldName = parser.getCurrentName();
      parser.nextValue();
      if ("content_type".equals(fieldName)) {
        contentType = parser.getValueAsString();
      }
      else if ("length".equals(fieldName)) {
        length = parser.getValueAsLong();
      }
      else if ("data".equals(fieldName)) {
        file = File.createTempFile("npm_attachment", "temp", temporaryDirectory);
        // TODO: can Jackson stream binary? I doubt...
        Files.write(parser.getBinaryValue(), file);
      }
    }
    checkArgument(file.length() == length, "Invalid content length!");
    return new PackageAttachment(name, file, contentType);
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

package com.bolyuba.nexus.plugin.npm.service.internal.orient;

import java.util.Map;

import com.bolyuba.nexus.plugin.npm.service.PackageRoot;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.dataformat.smile.SmileFactory;
import com.google.common.base.Charsets;
import com.google.common.base.Throwables;
import com.orientechnologies.orient.core.db.document.ODatabaseDocumentTx;
import com.orientechnologies.orient.core.metadata.schema.OClass;
import com.orientechnologies.orient.core.metadata.schema.OClass.INDEX_TYPE;
import com.orientechnologies.orient.core.metadata.schema.OSchema;
import com.orientechnologies.orient.core.metadata.schema.OType;
import com.orientechnologies.orient.core.record.impl.ODocument;

/**
 * {@link EntityHandler} for {@link PackageRoot} entity.
 */
public class PackageRootHandler
    extends EntityHandlerSupport<PackageRoot>
{
  private final ObjectMapper objectMapper;

  public PackageRootHandler(final ODatabaseDocumentTx db) {
    super(PackageRoot.class, db);
    this.objectMapper = new ObjectMapper(new SmileFactory());
  }

  @Override
  protected void createSchema(final OSchema schema, final OClass clazz) {
    clazz.createProperty("componentId", OType.STRING); // repositoryId:name
    clazz.createProperty("repositoryId", OType.STRING);
    clazz.createProperty("name", OType.STRING);
    clazz.createProperty("description", OType.STRING);
    clazz.createProperty("properties", OType.EMBEDDEDMAP, OType.STRING);
    clazz.createProperty("raw", OType.BINARY);
    clazz.createIndex(clazz.getName() + ".componentId", INDEX_TYPE.UNIQUE_HASH_INDEX, "componentId");
  }

  @Override
  public ODocument toDocument(final PackageRoot entity, final ODocument doc) {
    try {
      doc.field("componentId", entity.getComponentId());
      doc.field("repositoryId", entity.getRepositoryId());
      doc.field("name", entity.getName());
      doc.field("description", entity.getDescription());
      doc.field("properties", entity.getProperties());
      doc.field("raw", objectMapper.writeValueAsBytes(entity.getRaw()));
      return doc;
    }
    catch (Exception e) {
      throw Throwables.propagate(e);
    }
  }

  @Override
  public PackageRoot toEntity(final ODocument doc) {
    try {
      final String repositoryId = doc.field("repositoryId", OType.STRING);
      final Map<String, Object> raw = objectMapper.readValue(doc.<byte[]>field("raw", OType.BINARY),
          new TypeReference<Map<String, Object>>() {});
      final PackageRoot result = new PackageRoot(repositoryId, raw);
      final Map<String, String> properties = doc.field("properties", OType.EMBEDDEDMAP);
      result.getProperties().putAll(properties);
      return result;
    }
    catch (Exception e) {
      throw Throwables.propagate(e);
    }
  }
}

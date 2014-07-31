package com.bolyuba.nexus.plugin.npm.metadata.internal.orient;

import java.util.Map;

import com.bolyuba.nexus.plugin.npm.metadata.PackageRoot;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.google.common.base.Throwables;
import com.orientechnologies.orient.core.db.document.ODatabaseDocumentTx;
import com.orientechnologies.orient.core.metadata.schema.OClass;
import com.orientechnologies.orient.core.metadata.schema.OClass.INDEX_TYPE;
import com.orientechnologies.orient.core.metadata.schema.OSchema;
import com.orientechnologies.orient.core.metadata.schema.OType;
import com.orientechnologies.orient.core.record.impl.ODocument;

/**
 * Created by cstamas on 29/07/14.
 */
public class PackageRootHandler
    extends EntityHandlerSupport<PackageRoot>
{
  private final ObjectMapper objectMapper;

  public PackageRootHandler(final ODatabaseDocumentTx db) {
    super(PackageRoot.class, db);
    this.objectMapper = new ObjectMapper(); // TODO: consider compact binary SMILE format for "raw"
  }

  @Override
  protected void createSchema(final OSchema schema, final OClass clazz) {
    clazz.createProperty("componentId", OType.STRING); // repositoryId:name
    clazz.createProperty("repositoryId", OType.STRING);
    clazz.createProperty("name", OType.STRING);
    clazz.createProperty("description", OType.STRING);
    clazz.createProperty("raw", OType.STRING);
    clazz.createIndex(clazz.getName() + ".componentId", INDEX_TYPE.UNIQUE_HASH_INDEX, "componentId");
  }

  @Override
  public ODocument toDocument(final PackageRoot entity, final ODocument doc) {
    try {
      doc.field("componentId", entity.getComponentId());
      doc.field("repositoryId", entity.getRepositoryId());
      doc.field("name", entity.getName());
      doc.field("description", entity.getDescription());
      doc.field("raw", objectMapper.writeValueAsString(entity.getRaw()));
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
      final Map<String, Object> raw = objectMapper.readValue(doc.<String>field("raw", OType.STRING),
          new TypeReference<Map<String, Object>>() {});
      return new PackageRoot(repositoryId, raw);
    }
    catch (Exception e) {
      throw Throwables.propagate(e);
    }
  }
}

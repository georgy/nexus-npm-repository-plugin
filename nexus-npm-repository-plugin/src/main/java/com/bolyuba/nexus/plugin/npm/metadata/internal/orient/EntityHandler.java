package com.bolyuba.nexus.plugin.npm.metadata.internal.orient;

import com.orientechnologies.orient.core.metadata.schema.OClass;
import com.orientechnologies.orient.core.record.impl.ODocument;

/**
 * Bridge for OrientDB document and entities.
 */
public interface EntityHandler<T>
{
  /**
   * Returns the name of the schema as used in OrientDB. To be used to construct SQL queries or creating new document
   * instances.
   */
  String getSchemaName();

  /**
   * Returns the entity class handled by this handler.
   */
  Class<T> getJavaType();

  /**
   * Returns the orient class handled by this handler.
   */
  OClass getOrientType();

  /**
   * Translates the entity into Orient's {@link ODocument}.
   */
  ODocument toDocument(T entity, ODocument doc);

  /**
   * Translates Orient's {@link ODocument} to entity.
   */
  T toEntity(ODocument doc);
}

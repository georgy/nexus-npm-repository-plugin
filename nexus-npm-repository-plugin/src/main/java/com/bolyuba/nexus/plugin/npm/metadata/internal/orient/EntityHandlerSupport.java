package com.bolyuba.nexus.plugin.npm.metadata.internal.orient;

import java.util.Locale;

import com.orientechnologies.orient.core.db.document.ODatabaseDocumentTx;
import com.orientechnologies.orient.core.metadata.schema.OClass;
import com.orientechnologies.orient.core.metadata.schema.OSchema;

import static com.google.common.base.Preconditions.checkNotNull;

/**
 * Created by cstamas on 29/07/14.
 */
public abstract class EntityHandlerSupport<T>
    implements EntityHandler<T>
{
  private final Class<T> type;

  private final String schemaName;

  private final OClass otype;

  protected EntityHandlerSupport(final Class<T> type, final ODatabaseDocumentTx db) {
    this.type = checkNotNull(type);
    this.schemaName = type.getSimpleName().toLowerCase(Locale.US);
    this.otype = maybeCreateSchema(db);
  }

  @Override
  public String getSchemaName() {
    return schemaName;
  }

  @Override
  public Class<T> getJavaType() {
    return type;
  }

  @Override
  public OClass getOrientType() {
    return otype;
  }

  protected OClass maybeCreateSchema(final ODatabaseDocumentTx db) {
    final OSchema schema = db.getMetadata().getSchema();
    OClass clazz = schema.getClass(getSchemaName());
    if (clazz == null) {
      clazz = schema.createClass(getSchemaName());
      createSchema(schema, clazz);
    }
    return clazz;
  }

  protected abstract void createSchema(final OSchema schema, OClass clazz);

  @Override
  public String toString() {
    return getClass().getSimpleName() + "{" +
        "type=" + type +
        ", schemaName='" + schemaName + '\'' +
        '}';
  }
}

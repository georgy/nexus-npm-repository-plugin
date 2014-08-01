package com.bolyuba.nexus.plugin.npm.metadata.internal.orient;

import java.io.File;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import javax.annotation.Nullable;
import javax.inject.Inject;
import javax.inject.Named;
import javax.inject.Singleton;

import org.sonatype.nexus.configuration.application.ApplicationDirectories;
import org.sonatype.sisu.goodies.lifecycle.LifecycleSupport;

import com.bolyuba.nexus.plugin.npm.NpmRepository;
import com.bolyuba.nexus.plugin.npm.metadata.PackageRoot;
import com.bolyuba.nexus.plugin.npm.metadata.internal.MetadataStore;
import com.google.common.base.Function;
import com.google.common.collect.Lists;
import com.google.common.collect.Maps;
import com.orientechnologies.orient.core.db.document.ODatabaseDocumentPool;
import com.orientechnologies.orient.core.db.document.ODatabaseDocumentTx;
import com.orientechnologies.orient.core.id.ORID;
import com.orientechnologies.orient.core.id.ORecordId;
import com.orientechnologies.orient.core.intent.OIntentMassiveInsert;
import com.orientechnologies.orient.core.metadata.schema.OType;
import com.orientechnologies.orient.core.record.impl.ODocument;
import com.orientechnologies.orient.core.sql.query.OSQLSynchQuery;

import static com.google.common.base.Preconditions.checkArgument;
import static com.google.common.base.Preconditions.checkNotNull;

/**
 * OrientDB backed {@link MetadataStore} implementation.
 */
@Singleton
@Named("default")
public class OrientMetadataStore
    extends LifecycleSupport
    implements MetadataStore
{
  private static final String DB_LOCATION = "db/npm";

  private final File databaseDirectory;

  private final Map<Class<?>, EntityHandler<?>> entityHandlers;

  private ODatabaseDocumentPool pool;

  @Inject
  public OrientMetadataStore(final ApplicationDirectories applicationDirectories) {
    this.databaseDirectory = applicationDirectories.getWorkDirectory(DB_LOCATION);
    this.entityHandlers = Maps.newHashMap();
  }

  // Lifecycle
  @Override
  protected void doStart() throws Exception {
    final String dbUri = "plocal:" + databaseDirectory.getAbsolutePath();
    try (ODatabaseDocumentTx db = new ODatabaseDocumentTx(dbUri)) {
      if (!db.exists()) {
        db.create();
        log.info("Created database: {}", databaseDirectory);
      }
      else {
        db.open("admin", "admin");
        log.info("Opened database: {}", db);
      }
      registerHandler(new PackageRootHandler(db));
    }

    pool = new ODatabaseDocumentPool(dbUri, "admin", "admin");
    pool.setName("npm-database-pool");
    pool.setup(1, 10);
    log.info("Created pool: {}", pool);
  }

  @Override
  public void doStop() throws Exception {
    log.info("Closing pool: {}", pool);
    pool.close();
    pool = null;
  }

  /**
   * Acquires a pooled DB, close it to release back to pool.
   */
  private ODatabaseDocumentTx db() {
    ensureStarted();
    return pool.acquire();
  }

  @SuppressWarnings("unchecked")
  private <T> EntityHandler<T> getHandlerFor(Class<T> schema) {
    final EntityHandler<T> result = (EntityHandler<T>) entityHandlers.get(schema);
    checkArgument(result != null, "Schema %s has no registered handler!", schema.getName());
    return result;
  }

  private void registerHandler(final EntityHandler<?> entityHandler) {
    log.debug("Registering entity handler for type {}", entityHandler.getJavaType().getName());
    entityHandlers.put(entityHandler.getJavaType(), entityHandler);
  }

  // == API

  @Override
  public List<String> listPackageNames(final NpmRepository repository) {
    checkNotNull(repository);
    final EntityHandler<PackageRoot> entityHandler = getHandlerFor(PackageRoot.class);
    try (ODatabaseDocumentTx db = db()) {
      db.begin();
      try {
        final OSQLSynchQuery<ODocument> query = new OSQLSynchQuery<>(
            "select from " + entityHandler.getSchemaName() + " where @rid > ? limit 1000");
        ORID last = new ORecordId();
        final List<String> result = Lists.newArrayList();
        List<ODocument> resultset = db.query(query, last);
        while (!resultset.isEmpty()) {
          result.addAll(Lists.transform(resultset, new Function<ODocument, String>()
          {
            public String apply(@Nullable final ODocument input) {
              return input.field("name", OType.STRING);
            }
          }));
          last = resultset.get(resultset.size() - 1).getIdentity();
          resultset = db.query(query, last);
        }
        return result;
      }
      finally {
        db.commit();
      }
    }
  }

  @Override
  public PackageRoot getPackageByName(final NpmRepository repository, final String packageName) {
    checkNotNull(repository);
    checkNotNull(packageName);
    final EntityHandler<PackageRoot> entityHandler = getHandlerFor(PackageRoot.class);
    try (ODatabaseDocumentTx db = db()) {
      db.begin();
      try {
        final ODocument doc = doGetPackageByName(db, entityHandler, repository, packageName);
        if (doc == null) {
          return null;
        }
        return entityHandler.toEntity(doc);
      }
      finally {
        db.commit();
      }
    }
  }

  @Override
  public boolean deletePackageByName(final NpmRepository repository, final String packageName) {
    checkNotNull(repository);
    checkNotNull(packageName);
    final EntityHandler<PackageRoot> entityHandler = getHandlerFor(PackageRoot.class);
    try (ODatabaseDocumentTx db = db()) {
      db.begin();
      try {
        final ODocument doc = doGetPackageByName(db, entityHandler, repository, packageName);
        if (doc == null) {
          return false;
        }
        db.delete(doc);
        return true;
      }
      finally {
        db.commit();
      }
    }
  }

  @Override
  public PackageRoot updatePackage(final NpmRepository repository, final PackageRoot packageRoot) {
    checkNotNull(repository);
    checkNotNull(packageRoot);
    final EntityHandler<PackageRoot> entityHandler = getHandlerFor(PackageRoot.class);
    try (ODatabaseDocumentTx db = db()) {
      db.begin();
      try {
        return doUpdatePackage(db, entityHandler, repository, packageRoot);
      }
      finally {
        db.commit();
      }
    }
  }

  @Override
  public int updatePackages(final NpmRepository repository, final Iterator<PackageRoot> packageRootIterator) {
    checkNotNull(repository);
    checkNotNull(packageRootIterator);
    final EntityHandler<PackageRoot> entityHandler = getHandlerFor(PackageRoot.class);
    try (ODatabaseDocumentTx db = db()) {
      db.declareIntent(new OIntentMassiveInsert());
      try {
        int count = 0;
        while (packageRootIterator.hasNext()) {
          final PackageRoot packageRoot = packageRootIterator.next();
          db.begin();
          try {
            doUpdatePackage(db, entityHandler, repository, packageRoot);
          }
          finally {
            db.commit();
          }
          count++;
        }
        return count;
      }
      finally {
        db.declareIntent(null);
      }
    }
  }
  // ==

  private ODocument doGetPackageByName(final ODatabaseDocumentTx db,
                                       final EntityHandler<PackageRoot> entityHandler,
                                       final NpmRepository repository,
                                       final String packageName)
  {
    final List<ODocument> entries = db.query(new OSQLSynchQuery<>(
        "select * from " + entityHandler.getSchemaName() + " where componentId='" + repository.getId() + ":" +
            packageName + "'"));
    for (ODocument entry : entries) {
      return entry; // we expect only one
    }
    return null;
  }

  private PackageRoot doUpdatePackage(final ODatabaseDocumentTx db,
                                      final EntityHandler<PackageRoot> entityHandler,
                                      final NpmRepository repository,
                                      final PackageRoot packageRoot)
  {
    ODocument doc = doGetPackageByName(db, entityHandler, repository, packageRoot.getName());
    if (doc != null) {
      final PackageRoot existing = entityHandler.toEntity(doc);
      existing.overlay(packageRoot);
      db.save(entityHandler.toDocument(existing, doc));
      return existing;
    }
    else {
      doc = db.newInstance(entityHandler.getSchemaName());
      db.save(entityHandler.toDocument(packageRoot, doc));
      return packageRoot;
    }
  }

}

package com.bolyuba.nexus.plugin.npm.service.internal.orient;

import java.io.File;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import javax.annotation.Nullable;
import javax.inject.Inject;
import javax.inject.Named;
import javax.inject.Singleton;

import org.sonatype.nexus.configuration.application.ApplicationDirectories;
import org.sonatype.nexus.proxy.access.Action;
import org.sonatype.nexus.proxy.item.RepositoryItemUidLock;
import org.sonatype.sisu.goodies.lifecycle.LifecycleSupport;

import com.bolyuba.nexus.plugin.npm.NpmRepository;
import com.bolyuba.nexus.plugin.npm.service.PackageRoot;
import com.bolyuba.nexus.plugin.npm.service.internal.MetadataStore;
import com.google.common.base.Function;
import com.google.common.base.Predicate;
import com.google.common.base.Throwables;
import com.google.common.collect.Lists;
import com.google.common.collect.Maps;
import com.orientechnologies.orient.core.config.OGlobalConfiguration;
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
        // latest advice is to disable DB compression as it doesn't buy much,
        // also snappy has issues with use of native lib (unpacked under tmp)
        OGlobalConfiguration.STORAGE_COMPRESSION_METHOD.setValue("nothing");
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
            "select from " + entityHandler.getSchemaName() + " where repositoryId='" + repository.getId() +
                "' and @rid > ? limit 1000");
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
    // TODO: reuse existing UID lock "/packageName" or introduce new one "/npmMetadata/packageName"?
    final RepositoryItemUidLock lock = repository.createUid(packageRoot.getName()).getLock();
    lock.lock(Action.update);
    try (ODatabaseDocumentTx db = db()) {
      db.begin();
      try {
        return doUpdatePackage(db, entityHandler, repository, packageRoot);
      }
      finally {
        db.commit();
      }
    }
    finally {
      lock.unlock();
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
        db.begin();
        try {
          while (packageRootIterator.hasNext()) {
            final PackageRoot packageRoot = packageRootIterator.next();
            doUpdatePackage(db, entityHandler, repository, packageRoot);
            count++;
          }
          db.commit();
        }
        catch (Exception e) {
          db.rollback();
          throw Throwables.propagate(e);
        }
        return count;
      }
      finally {
        db.declareIntent(null);
      }
    }
  }

  @Override
  public int updatePackages(final NpmRepository repository, final Predicate<PackageRoot> predicate,
                            final Function<PackageRoot, PackageRoot> function)
  {
    checkNotNull(repository);
    checkNotNull(function);
    final EntityHandler<PackageRoot> entityHandler = getHandlerFor(PackageRoot.class);
    try (ODatabaseDocumentTx db = db()) {
      db.begin();
      try {
        final OSQLSynchQuery<ODocument> query = new OSQLSynchQuery<>(
            "select from " + entityHandler.getSchemaName() + " where repositoryId='" + repository.getId() +
                "' and @rid > ? limit 1000");
        ORID last = new ORecordId();
        List<ODocument> resultset = db.query(query, last);
        int count = 0;
        while (!resultset.isEmpty()) {
          for (ODocument doc : resultset) {
            PackageRoot root = entityHandler.toEntity(doc);
            if (predicate != null && !predicate.apply(root)) {
              continue;
            }
            root = function.apply(root);
            db.save(entityHandler.toDocument(root, doc));
            count++;
          }
          last = resultset.get(resultset.size() - 1).getIdentity();
          resultset = db.query(query, last);
        }
        return count;
      }
      finally {
        db.commit();
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

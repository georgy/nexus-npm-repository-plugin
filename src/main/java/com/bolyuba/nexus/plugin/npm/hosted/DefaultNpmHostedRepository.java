package com.bolyuba.nexus.plugin.npm.hosted;

import com.bolyuba.nexus.plugin.npm.NpmContentClass;
import com.bolyuba.nexus.plugin.npm.NpmRepository;
import com.bolyuba.nexus.plugin.npm.NpmUtility;
import com.bolyuba.nexus.plugin.npm.content.NpmJsonReader;
import com.bolyuba.nexus.plugin.npm.content.NpmMimeRulesSource;
import com.bolyuba.nexus.plugin.npm.hosted.content.NpmJsonContentLocator;
import com.bolyuba.nexus.plugin.npm.pkg.InvalidPackageRequestException;
import com.bolyuba.nexus.plugin.npm.pkg.PackageRequest;
import com.google.gson.Gson;
import com.google.gson.stream.JsonToken;
import org.apache.commons.codec.binary.Base64InputStream;
import org.codehaus.plexus.util.xml.Xpp3Dom;
import org.sonatype.inject.Description;
import org.sonatype.nexus.configuration.Configurator;
import org.sonatype.nexus.configuration.model.CRepository;
import org.sonatype.nexus.configuration.model.CRepositoryExternalConfigurationHolderFactory;
import org.sonatype.nexus.mime.MimeRulesSource;
import org.sonatype.nexus.proxy.AccessDeniedException;
import org.sonatype.nexus.proxy.IllegalOperationException;
import org.sonatype.nexus.proxy.ItemNotFoundException;
import org.sonatype.nexus.proxy.LocalStorageException;
import org.sonatype.nexus.proxy.ResourceStoreRequest;
import org.sonatype.nexus.proxy.StorageException;
import org.sonatype.nexus.proxy.access.Action;
import org.sonatype.nexus.proxy.item.AbstractStorageItem;
import org.sonatype.nexus.proxy.item.DefaultStorageFileItem;
import org.sonatype.nexus.proxy.item.PreparedContentLocator;
import org.sonatype.nexus.proxy.item.RepositoryItemUid;
import org.sonatype.nexus.proxy.item.RepositoryItemUidLock;
import org.sonatype.nexus.proxy.item.StorageFileItem;
import org.sonatype.nexus.proxy.item.StorageItem;
import org.sonatype.nexus.proxy.registry.ContentClass;
import org.sonatype.nexus.proxy.repository.AbstractRepository;
import org.sonatype.nexus.proxy.repository.DefaultRepositoryKind;
import org.sonatype.nexus.proxy.repository.RepositoryKind;
import org.sonatype.nexus.proxy.storage.UnsupportedStorageOperationException;

import javax.annotation.Nonnull;
import javax.inject.Inject;
import javax.inject.Named;
import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.Map;

import static com.google.common.base.Preconditions.checkNotNull;

/**
 * @author Georgy Bolyuba (georgy@bolyuba.com)
 */
@Named(DefaultNpmHostedRepository.ROLE_HINT)
@Description("Npm registry hosted repo")
public class DefaultNpmHostedRepository
        extends AbstractRepository
        implements NpmHostedRepository, NpmRepository {

    public static final String ROLE_HINT = "npm-hosted";

    /**
     * Hidden path to store uploaded publish requests before they are processed
     */
    private static final String PUBLISH_CACHE_PREFIX = RepositoryItemUid.PATH_SEPARATOR + ".publish";

    private final ContentClass contentClass;

    private final NpmHostedRepositoryConfigurator configurator;

    private final RepositoryKind repositoryKind;

    private final NpmMimeRulesSource mimeRulesSource;

    private final NpmUtility utility;

    @Inject
    public DefaultNpmHostedRepository(final @Named(NpmContentClass.ID) ContentClass contentClass,
                                      final NpmHostedRepositoryConfigurator configurator,
                                      final NpmUtility utility) {

        this.utility = checkNotNull(utility);
        this.mimeRulesSource = new NpmMimeRulesSource();
        this.contentClass = checkNotNull(contentClass);
        this.configurator = checkNotNull(configurator);
        this.repositoryKind = new DefaultRepositoryKind(NpmHostedRepository.class, null);
    }

    @Override
    protected Configurator getConfigurator() {
        return this.configurator;
    }

    @Override
    public RepositoryKind getRepositoryKind() {
        return this.repositoryKind;
    }

    @Override
    public ContentClass getRepositoryContentClass() {
        return this.contentClass;
    }

    @Override
    public MimeRulesSource getMimeRulesSource() {
        return mimeRulesSource;
    }

    @Override
    protected CRepositoryExternalConfigurationHolderFactory<?> getExternalConfigurationHolderFactory() {
        return new CRepositoryExternalConfigurationHolderFactory<NpmHostedRepositoryConfiguration>() {
            @Override
            public NpmHostedRepositoryConfiguration createExternalConfigurationHolder(final CRepository config) {
                return new NpmHostedRepositoryConfiguration((Xpp3Dom) config.getExternalConfiguration());
            }
        };
    }

    @Override
    protected AbstractStorageItem doRetrieveLocalItem(ResourceStoreRequest storeRequest) throws ItemNotFoundException, LocalStorageException {
        // only care about request if it is coming from npm client
        if (utility.isNmpRequest(storeRequest)) {
            try {
                PackageRequest packageRequest = new PackageRequest(storeRequest);
                if (packageRequest.isPackage()) {
                    return delegateDoRetrieveLocalItem(utility.replaceRequest(storeRequest));
                } else {
                    // huh?
                    return delegateDoRetrieveLocalItem(storeRequest);
                }
            } catch (InvalidPackageRequestException ignore) {
                return delegateDoRetrieveLocalItem(storeRequest);
            }
        } else {
            return delegateDoRetrieveLocalItem(storeRequest);
        }
    }

    AbstractStorageItem delegateDoRetrieveLocalItem(ResourceStoreRequest storeRequest) throws LocalStorageException, ItemNotFoundException {
        return super.doRetrieveLocalItem(storeRequest);
    }

    @SuppressWarnings("deprecation")
    @Override
    public void storeItem(ResourceStoreRequest request, InputStream is, Map<String, String> userAttributes)
            throws UnsupportedStorageOperationException, IllegalOperationException, StorageException, AccessDeniedException {
        try {
            PackageRequest packageRequest = new PackageRequest(request);

            if (!packageRequest.isPackageRoot()) {
                throw new InvalidRegistryOperationException("Store operations are only valid for package roots");
            }
            // serialize all publish request for the same
            final RepositoryItemUid publisherUid = createUid(packageRequest.getPath() + ".publish()");
            RepositoryItemUidLock publisherLock = publisherUid.getLock();

            publisherLock.lock(Action.create);
            try {
                //store item in cache for parsing
                final ResourceStoreRequest publishCacheRequest = getPublishCacheRequest(request);
                super.storeItem(publishCacheRequest, is, userAttributes);

                try {
                    final AbstractStorageItem abstractStorageItem = super.doRetrieveLocalItem(publishCacheRequest);
                    if (!StorageFileItem.class.isInstance(abstractStorageItem)) {
                        throw new LocalStorageException("Publish request was not stored as a file");
                    }
                    StorageFileItem publishRequest = (StorageFileItem) abstractStorageItem;

                    // got publish request on disk, slice it and dice it as we see fit!
                    try {
                        Gson gson = new Gson();
                        try {
                            try (InputStream inputStream = publishRequest.getInputStream()) {
                                PublishVersionObject pvo = gson.fromJson(new InputStreamReader(inputStream), PublishVersionObject.class);
                                final String versionKey = getVersionKey(pvo.versions);
                                validateVersion(packageRequest, pvo.versions, versionKey);
                                this.storeItem(false, getStorageItemForVersion(packageRequest, versionKey, gson.toJson(pvo.versions.get(versionKey))));
                            }

                            try (InputStream in = publishRequest.getInputStream()) {
                                NpmJsonReader attachmentsReader = getAttachments(in);
                                if (attachmentsReader != null) {
                                    final String attachmentName = attachmentsReader.nextName();
                                    InputStream attachmentStream = getAttachmentStream(attachmentsReader);
                                    this.storeItem(false, getStorageItemForAttachment(packageRequest, attachmentName, attachmentStream));
                                }
                            }
                        } catch (IOException e) {
                            throw new LocalStorageException("Error reading publish request form the file", e);
                        }
                    } finally {
                        // regardless of passing results try to clean publish request
                        try {
                            // we exclusively lock uploaded publish request
                            this.getLocalStorage().deleteItem(this, publishCacheRequest);
                        } catch (UnsupportedStorageOperationException | ItemNotFoundException | StorageException ignore) {
                            // we gave it best shot! not going to clean my room (sorry mom)
                        }
                    }
                } catch (ItemNotFoundException e) {
                    throw new LocalStorageException("Cannot find publish request we just stored!", e);
                }
            } finally {
                publisherLock.unlock();
            }
        } catch (InvalidPackageRequestException e) {
            // TODO: This might be our tarball, but it also might be something stupid uploaded. Need to validate further
            // for now just store it
            super.storeItem(request, is, userAttributes);
        }
    }


    ResourceStoreRequest getPublishCacheRequest(ResourceStoreRequest originalRequest) {
        return new ResourceStoreRequest(PUBLISH_CACHE_PREFIX + originalRequest.getRequestPath());
    }

    String getVersionKey(Map<String, Object> versions) throws LocalStorageException {
        if (versions == null || versions.isEmpty()) {
            throw new LocalStorageException("No versions were found in publish request");
        }
        if (versions.size() != 1) {
            throw new LocalStorageException("Cannot handle publish request with multiple versions");
        }
        return versions.keySet().iterator().next();
    }

    void validateVersion(PackageRequest packageRequest, Map<String, Object> versions, String versionKey) throws LocalStorageException {

        final Object o = versions.get(versionKey);

        if (o == null || !Map.class.isInstance(o)) {
            throw new LocalStorageException("Cannot handle publish request: version key " + versionKey + " content cannot be interpreted");
        }

        @SuppressWarnings("unchecked")
        Map<String, Object> items = (Map) versions.get(versionKey);

        final String packageName = extractStringValue(versionKey, items, "name");
        if (!packageName.equals(packageRequest.getName())) {
            throw new LocalStorageException("Cannot publish package (names do not match). Package [" + packageName + "], request: " + packageRequest);
        }
        final String packageVersion = extractStringValue(versionKey, items, "version");
        if (!versionKey.equals(packageVersion)) {
            throw new LocalStorageException("Cannot publish package (version does not match version key). Version key [" + versionKey + "], request: " + packageRequest);
        }
    }

    String extractStringValue(String versionKey, Map<String, Object> items, String valueName) throws LocalStorageException {
        if (!items.containsKey(valueName)) {
            throw new LocalStorageException("Cannot handle publish request: version key " + versionKey + " does not have value [" + valueName + "]");
        }
        final Object obj = items.get(valueName);

        if (obj == null || !String.class.isInstance(obj)) {
            throw new LocalStorageException("Cannot handle publish request: version key " + versionKey + " value of [" + valueName + "] cannot be interpreted");
        }
        return (String) obj;
    }

    StorageItem getStorageItemForVersion(@Nonnull PackageRequest packageRequest, @Nonnull String version, @Nonnull String json) {
        final String requestContentCachePath = getRequestContentCachePath(packageRequest.getPath() + RepositoryItemUid.PATH_SEPARATOR + version);
        ResourceStoreRequest resourceStoreRequest = new ResourceStoreRequest(requestContentCachePath);

        return new DefaultStorageFileItem(this,
                resourceStoreRequest,
                true,
                true,
                new NpmJsonContentLocator(json));
    }

    StorageItem getStorageItemForAttachment(@Nonnull PackageRequest packageRequest, @Nonnull String attachmentName, @Nonnull InputStream inputStream) {
        final String requestContentCachePath = packageRequest.getPath() + RepositoryItemUid.PATH_SEPARATOR + NPM_REGISTRY_SPECIAL +
                RepositoryItemUid.PATH_SEPARATOR + attachmentName;
        ResourceStoreRequest resourceStoreRequest = new ResourceStoreRequest(requestContentCachePath);

        return new DefaultStorageFileItem(this,
                resourceStoreRequest,
                true,
                true,
                new PreparedContentLocator(inputStream, NpmRepository.TARBALL_MIME_TYPE, 0));
    }

    String getRequestContentCachePath(@Nonnull String path) {
        if (!path.endsWith(RepositoryItemUid.PATH_SEPARATOR)) {
            path = path + RepositoryItemUid.PATH_SEPARATOR;
        }
        return path + JSON_CONTENT_FILE_NAME;
    }

    NpmJsonReader getAttachments(InputStream in) {
        NpmJsonReader jsonReader = new NpmJsonReader(in);
        try {
            if (!jsonReader.skipToName("_attachments")) {
                jsonReader.close();
                return null;
            } else {
                JsonToken peek = jsonReader.peek();
                if (peek != JsonToken.BEGIN_OBJECT) {
                    jsonReader.close();
                    return null;
                }
                jsonReader.beginObject();

                // check if there is a name and not obj end, etc
                peek = jsonReader.peek();
                if (peek == JsonToken.NAME) {
                    return jsonReader;
                } else {
                    jsonReader.close();
                    return null;
                }
            }
        } catch (IOException e) {
            return null;
        }
    }

    InputStream getAttachmentStream(NpmJsonReader jsonReader) throws IOException {
        if (!jsonReader.skipToName("data")) {
            return null;
        }
        return new Base64InputStream(new ByteArrayInputStream(jsonReader.nextString().getBytes()));
    }
}
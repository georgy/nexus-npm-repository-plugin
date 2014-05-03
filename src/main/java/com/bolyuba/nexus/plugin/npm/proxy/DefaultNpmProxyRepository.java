package com.bolyuba.nexus.plugin.npm.proxy;

import com.bolyuba.nexus.plugin.npm.NpmContentClass;
import com.bolyuba.nexus.plugin.npm.NpmRepository;
import com.bolyuba.nexus.plugin.npm.NpmUtility;
import com.bolyuba.nexus.plugin.npm.pkg.InvalidPackageRequestException;
import com.bolyuba.nexus.plugin.npm.pkg.PackageRequest;
import com.bolyuba.nexus.plugin.npm.proxy.content.NpmFilteringContentLocator;
import com.bolyuba.nexus.plugin.npm.proxy.content.NpmMimeRulesSource;
import com.bolyuba.nexus.plugin.npm.storage.NpmLocalStorageWrapper;
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
import org.sonatype.nexus.proxy.item.AbstractStorageItem;
import org.sonatype.nexus.proxy.item.DefaultStorageFileItem;
import org.sonatype.nexus.proxy.item.RepositoryItemUid;
import org.sonatype.nexus.proxy.item.StorageItem;
import org.sonatype.nexus.proxy.registry.ContentClass;
import org.sonatype.nexus.proxy.repository.AbstractProxyRepository;
import org.sonatype.nexus.proxy.repository.DefaultRepositoryKind;
import org.sonatype.nexus.proxy.repository.RepositoryKind;
import org.sonatype.nexus.proxy.storage.local.LocalRepositoryStorage;

import javax.inject.Inject;
import javax.inject.Named;

import static com.google.common.base.Preconditions.checkNotNull;

/**
 * @author Georgy Bolyuba (georgy@bolyuba.com)
 */
@Named(DefaultNpmProxyRepository.ROLE_HINT)
@Description("Npm registry proxy repo")
public class DefaultNpmProxyRepository
        extends AbstractProxyRepository
        implements NpmProxyRepository, NpmRepository {

    public static final String ROLE_HINT = "npm-proxy";

    private final ContentClass contentClass;

    private final NpmProxyRepositoryConfigurator configurator;

    private final RepositoryKind repositoryKind;

    private final NpmMimeRulesSource mimeRulesSource;

    private final NpmUtility utility;

    @Inject
    public DefaultNpmProxyRepository(final @Named(NpmContentClass.ID) ContentClass contentClass,
                                     final NpmProxyRepositoryConfigurator configurator,
                                     final NpmMimeRulesSource mimeRulesSource,
                                     final NpmUtility utility) {

        this.contentClass = checkNotNull(contentClass);
        this.configurator = checkNotNull(configurator);
        this.mimeRulesSource = checkNotNull(mimeRulesSource);
        this.utility = checkNotNull(utility);
        this.repositoryKind = new DefaultRepositoryKind(NpmProxyRepository.class, null);
    }

    @Override
    protected Configurator getConfigurator() {
        return configurator;
    }

    @Override
    public RepositoryKind getRepositoryKind() {
        return repositoryKind;
    }

    @Override
    public ContentClass getRepositoryContentClass() {
        return contentClass;
    }

    @Override
    protected CRepositoryExternalConfigurationHolderFactory<NpmProxyRepositoryConfiguration> getExternalConfigurationHolderFactory() {
        return new CRepositoryExternalConfigurationHolderFactory<NpmProxyRepositoryConfiguration>() {
            @Override
            public NpmProxyRepositoryConfiguration createExternalConfigurationHolder(final CRepository config) {
                return new NpmProxyRepositoryConfiguration((Xpp3Dom) config.getExternalConfiguration());
            }
        };
    }

    @Override
    public void setLocalStorage(LocalRepositoryStorage localStorage) {
        LocalRepositoryStorage wrapper = new NpmLocalStorageWrapper(localStorage, utility);
        super.setLocalStorage(wrapper);
    }

    @Override
    public MimeRulesSource getMimeRulesSource() {
        return mimeRulesSource;
    }

    @SuppressWarnings("deprecation")
    @Override
    public StorageItem retrieveItem(ResourceStoreRequest storeRequest) throws IllegalOperationException, ItemNotFoundException, StorageException, AccessDeniedException {
        return super.retrieveItem(storeRequest);
    }

    @Override
    public AbstractStorageItem doCacheItem(AbstractStorageItem item) throws LocalStorageException {
        try {
            ResourceStoreRequest storeRequest = item.getResourceStoreRequest();
            PackageRequest packageRequest = new PackageRequest(storeRequest);

            if (packageRequest.isPackage()) {
                DefaultStorageFileItem wrappedItem = wrapItem((DefaultStorageFileItem) item);
                return delegateDoCacheItem(wrappedItem);
            } else {
                return item;
            }
        } catch (InvalidPackageRequestException ignore) {
            // not something we are interested in
        }

        return delegateDoCacheItem(item);
    }

    // let test mock this method
    AbstractStorageItem delegateDoCacheItem(AbstractStorageItem item) throws LocalStorageException {
        return super.doCacheItem(item);
    }

    DefaultStorageFileItem wrapItem(DefaultStorageFileItem item) {
        ResourceStoreRequest request = item.getResourceStoreRequest();

        NpmFilteringContentLocator decoratedContentLocator =
                new NpmFilteringContentLocator(item.getContentLocator(), request, this.getRemoteUrl());

        String path = request.getRequestPath();
        if (!path.endsWith(RepositoryItemUid.PATH_SEPARATOR)) {
            path = path + RepositoryItemUid.PATH_SEPARATOR;
        }
        request.setRequestPath(path + JSON_CONTENT_FILE_NAME);

        return getWrappedStorageFileItem(item, decoratedContentLocator, request);
    }

    // let test mock this method
    DefaultStorageFileItem getWrappedStorageFileItem(DefaultStorageFileItem item, NpmFilteringContentLocator decoratedContentLocator, ResourceStoreRequest decoratedRequest) {
        return new DefaultStorageFileItem(
                this,
                    decoratedRequest,
                    item.isReadable(),
                    item.isWritable(),
                    decoratedContentLocator);
    }
}

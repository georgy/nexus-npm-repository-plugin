package com.bolyuba.nexus.plugin.npm.proxy;

import com.bolyuba.nexus.plugin.npm.NpmContentClass;
import com.bolyuba.nexus.plugin.npm.NpmRepository;
import com.bolyuba.nexus.plugin.npm.NpmUtility;
import com.bolyuba.nexus.plugin.npm.content.NpmFilteringContentLocator;
import com.bolyuba.nexus.plugin.npm.content.NpmMimeRulesSource;
import com.bolyuba.nexus.plugin.npm.pkg.InvalidPackageRequestException;
import com.bolyuba.nexus.plugin.npm.pkg.PackageRequest;
import org.codehaus.plexus.util.xml.Xpp3Dom;
import org.sonatype.inject.Description;
import org.sonatype.nexus.configuration.Configurator;
import org.sonatype.nexus.configuration.model.CRepository;
import org.sonatype.nexus.configuration.model.CRepositoryExternalConfigurationHolderFactory;
import org.sonatype.nexus.mime.MimeRulesSource;
import org.sonatype.nexus.proxy.ItemNotFoundException;
import org.sonatype.nexus.proxy.LocalStorageException;
import org.sonatype.nexus.proxy.ResourceStoreRequest;
import org.sonatype.nexus.proxy.item.AbstractStorageItem;
import org.sonatype.nexus.proxy.item.DefaultStorageFileItem;
import org.sonatype.nexus.proxy.registry.ContentClass;
import org.sonatype.nexus.proxy.repository.AbstractProxyRepository;
import org.sonatype.nexus.proxy.repository.DefaultRepositoryKind;
import org.sonatype.nexus.proxy.repository.RepositoryKind;

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
                                     final NpmUtility npmUtility) {

        this.utility = checkNotNull(npmUtility);
        this.contentClass = checkNotNull(contentClass);
        this.configurator = checkNotNull(configurator);

        this.repositoryKind = new DefaultRepositoryKind(NpmProxyRepository.class, null);
        this.mimeRulesSource = new NpmMimeRulesSource();
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
    public MimeRulesSource getMimeRulesSource() {
        return mimeRulesSource;
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

    @Override
    public AbstractStorageItem doCacheItem(AbstractStorageItem item) throws LocalStorageException {
        try {
            ResourceStoreRequest storeRequest = item.getResourceStoreRequest();
            PackageRequest packageRequest = new PackageRequest(storeRequest);

            if (packageRequest.isPackage()) {
                DefaultStorageFileItem wrappedItem = wrapItem((DefaultStorageFileItem) item);
                return delegateDoCacheItem(wrappedItem);
            } else {
                // no cache 4 u
                return item;
            }
        } catch (InvalidPackageRequestException ignore) {
            // do it old style
            return delegateDoCacheItem(item);
        }
    }

    DefaultStorageFileItem wrapItem(DefaultStorageFileItem item) {
        ResourceStoreRequest request = item.getResourceStoreRequest();

        NpmFilteringContentLocator decoratedContentLocator =
                new NpmFilteringContentLocator(item.getContentLocator(), request, this.getRemoteUrl());

        utility.wrapRequest(request);

        return getWrappedStorageFileItem(item, decoratedContentLocator, request);
    }

    // tests to mock these methods
    DefaultStorageFileItem getWrappedStorageFileItem(DefaultStorageFileItem item, NpmFilteringContentLocator decoratedContentLocator, ResourceStoreRequest decoratedRequest) {
        return new DefaultStorageFileItem(
                this,
                decoratedRequest,
                item.isReadable(),
                item.isWritable(),
                decoratedContentLocator);
    }

    AbstractStorageItem delegateDoCacheItem(AbstractStorageItem item) throws LocalStorageException {
        return super.doCacheItem(item);
    }

    AbstractStorageItem delegateDoRetrieveLocalItem(ResourceStoreRequest storeRequest) throws LocalStorageException, ItemNotFoundException {
        return super.doRetrieveLocalItem(storeRequest);
    }
}
package com.bolyuba.nexus.plugin.npm.proxy;

import com.bolyuba.nexus.plugin.npm.NpmContentClass;
import com.bolyuba.nexus.plugin.npm.proxy.storage.NpmLocalStorageWrapper;
import com.bolyuba.nexus.plugin.npm.proxy.storage.NpmRemoteStorageWrapper;
import org.codehaus.plexus.util.xml.Xpp3Dom;
import org.sonatype.inject.Description;
import org.sonatype.nexus.configuration.Configurator;
import org.sonatype.nexus.configuration.model.CRepository;
import org.sonatype.nexus.configuration.model.CRepositoryExternalConfigurationHolderFactory;
import org.sonatype.nexus.proxy.*;
import org.sonatype.nexus.proxy.item.AbstractStorageItem;
import org.sonatype.nexus.proxy.item.RepositoryItemUid;
import org.sonatype.nexus.proxy.item.StorageItem;
import org.sonatype.nexus.proxy.registry.ContentClass;
import org.sonatype.nexus.proxy.repository.AbstractProxyRepository;
import org.sonatype.nexus.proxy.repository.DefaultRepositoryKind;
import org.sonatype.nexus.proxy.repository.Repository;
import org.sonatype.nexus.proxy.repository.RepositoryKind;
import org.sonatype.nexus.proxy.storage.local.LocalRepositoryStorage;
import org.sonatype.nexus.proxy.storage.remote.RemoteRepositoryStorage;

import javax.inject.Inject;
import javax.inject.Named;

import java.util.regex.Pattern;

import static com.google.common.base.Preconditions.checkNotNull;

/**
 * @author Georgy Bolyuba (georgy@bolyuba.com)
 */
@Named(DefaultNpmProxyRepository.ROLE_HINT)
@Description("Nodejs npmjs.org repository")
public class DefaultNpmProxyRepository
        extends AbstractProxyRepository
        implements NpmProxyRepository {

    public static final String ROLE_HINT = "npm";

    private final ContentClass contentClass;

    private final NpmProxyRepositoryConfigurator configurator;

    private final RepositoryKind repositoryKind;

    @Inject
    public DefaultNpmProxyRepository(final @Named(NpmContentClass.ID) ContentClass contentClass,
                                     final NpmProxyRepositoryConfigurator configurator) {

        this.contentClass = checkNotNull(contentClass);
        this.configurator = checkNotNull(configurator);
        this.repositoryKind = new DefaultRepositoryKind(NpmProxyRepository.class, null);
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
    public void setLocalStorage(LocalRepositoryStorage localStorage) {
        LocalRepositoryStorage wrapper = new NpmLocalStorageWrapper(localStorage);
        super.setLocalStorage(wrapper);
    }

    @Override
    public void setRemoteStorage(RemoteRepositoryStorage remoteStorage) {
        RemoteRepositoryStorage wrapper = new NpmRemoteStorageWrapper(remoteStorage);
        super.setRemoteStorage(wrapper);
    }

    @Override
    public StorageItem retrieveItem(ResourceStoreRequest request) throws IllegalOperationException, ItemNotFoundException, StorageException, AccessDeniedException {
        // TODO: This does not work yet, always returns full "all"
        if ("/-/all/since".equals(request.getRequestPath())) {
            request.setRequestRemoteOnly(true);
        }
        return super.retrieveItem(request);
    }
}

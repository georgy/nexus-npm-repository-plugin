package com.bolyuba.nexus.plugin.npm.proxy;

import com.bolyuba.nexus.plugin.npm.NpmContentClass;
import com.bolyuba.nexus.plugin.npm.NpmPlugin;
import com.bolyuba.nexus.plugin.npm.proxy.content.NpmMimeRulesSource;
import com.bolyuba.nexus.plugin.npm.proxy.storage.NpmLocalStorageWrapper;
import com.bolyuba.nexus.plugin.npm.proxy.storage.NpmRemoteStorageWrapper;
import org.codehaus.plexus.util.xml.Xpp3Dom;
import org.sonatype.inject.Description;
import org.sonatype.nexus.configuration.Configurator;
import org.sonatype.nexus.configuration.model.CRepository;
import org.sonatype.nexus.configuration.model.CRepositoryExternalConfigurationHolderFactory;
import org.sonatype.nexus.mime.MimeRulesSource;
import org.sonatype.nexus.proxy.AccessDeniedException;
import org.sonatype.nexus.proxy.IllegalOperationException;
import org.sonatype.nexus.proxy.ItemNotFoundException;
import org.sonatype.nexus.proxy.ResourceStoreRequest;
import org.sonatype.nexus.proxy.StorageException;
import org.sonatype.nexus.proxy.item.StorageItem;
import org.sonatype.nexus.proxy.registry.ContentClass;
import org.sonatype.nexus.proxy.repository.AbstractProxyRepository;
import org.sonatype.nexus.proxy.repository.DefaultRepositoryKind;
import org.sonatype.nexus.proxy.repository.RepositoryKind;
import org.sonatype.nexus.proxy.storage.local.LocalRepositoryStorage;
import org.sonatype.nexus.proxy.storage.remote.RemoteRepositoryStorage;

import javax.inject.Inject;
import javax.inject.Named;

import static com.google.common.base.Preconditions.checkNotNull;

/**
 * @author Georgy Bolyuba (georgy@bolyuba.com)
 */
@Named(NpmPlugin.ROLE_HINT)
@Description("Npm registry proxy repo")
public class DefaultNpmProxyRepository
        extends AbstractProxyRepository
        implements NpmProxyRepository {

    private final ContentClass contentClass;

    private final NpmProxyRepositoryConfigurator configurator;

    private final RepositoryKind repositoryKind;

    private final NpmMimeRulesSource mimeRulesSource;

    private final NpmUtility utility;

    @Inject
    public DefaultNpmProxyRepository(final @Named(NpmContentClass.ID) ContentClass contentClass,
                                     final NpmProxyRepositoryConfigurator configurator,
                                     final NpmMimeRulesSource mimeRulesSource,
                                     NpmUtility utility) {

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
    public void setRemoteStorage(RemoteRepositoryStorage remoteStorage) {
        RemoteRepositoryStorage wrapper = new NpmRemoteStorageWrapper(remoteStorage, utility);
        super.setRemoteStorage(wrapper);
    }

    @Override
    public MimeRulesSource getMimeRulesSource() {
        return mimeRulesSource;
    }

    @SuppressWarnings("deprecation")
    @Override
    public StorageItem retrieveItem(ResourceStoreRequest request) throws IllegalOperationException, ItemNotFoundException, StorageException, AccessDeniedException {
        if (utility.shouldNotCache(request)) {
            request.setRequestRemoteOnly(true);
        }
        if (utility.shouldNotGotRemote(request)) {
            request.setRequestLocalOnly(true);
        }
        return super.retrieveItem(request);
    }
}

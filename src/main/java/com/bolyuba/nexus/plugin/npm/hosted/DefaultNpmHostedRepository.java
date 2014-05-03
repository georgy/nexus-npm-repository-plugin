package com.bolyuba.nexus.plugin.npm.hosted;

import com.bolyuba.nexus.plugin.npm.NpmContentClass;
import com.bolyuba.nexus.plugin.npm.NpmRepository;
import com.bolyuba.nexus.plugin.npm.NpmUtility;
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
import org.sonatype.nexus.proxy.ResourceStoreRequest;
import org.sonatype.nexus.proxy.StorageException;
import org.sonatype.nexus.proxy.item.DefaultStorageFileItem;
import org.sonatype.nexus.proxy.item.StorageItem;
import org.sonatype.nexus.proxy.registry.ContentClass;
import org.sonatype.nexus.proxy.repository.AbstractRepository;
import org.sonatype.nexus.proxy.repository.DefaultRepositoryKind;
import org.sonatype.nexus.proxy.repository.RepositoryKind;
import org.sonatype.nexus.proxy.storage.UnsupportedStorageOperationException;
import org.sonatype.nexus.proxy.storage.local.LocalRepositoryStorage;

import javax.inject.Inject;
import javax.inject.Named;
import java.io.InputStream;
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

    private final ContentClass contentClass;

    private final NpmHostedRepositoryConfigurator configurator;

    private final RepositoryKind repositoryKind;

    private final NpmMimeRulesSource mimeRulesSource;

    private final NpmUtility utility;

    @Inject
    public DefaultNpmHostedRepository(final @Named(NpmContentClass.ID) ContentClass contentClass,
                                      final NpmHostedRepositoryConfigurator configurator,
                                      final NpmUtility utility,
                                      NpmMimeRulesSource mimeRulesSource) {

        this.mimeRulesSource = checkNotNull(mimeRulesSource);
        this.contentClass = checkNotNull(contentClass);
        this.configurator = checkNotNull(configurator);
        this.utility = checkNotNull(utility);
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
    public void setLocalStorage(LocalRepositoryStorage localStorage) {
        LocalRepositoryStorage wrapper = new NpmLocalStorageWrapper(localStorage, utility);
        super.setLocalStorage(wrapper);
    }

    @SuppressWarnings("deprecation")
    @Override
    public void storeItem(ResourceStoreRequest request, InputStream is, Map<String, String> userAttributes)
            throws UnsupportedStorageOperationException, IllegalOperationException, StorageException, AccessDeniedException {

        utility.addNpmMeta(request);

        ResourceStoreRequest hiddenRequest = utility.hideInCache(request);
        try {
            // this needs to be synchronized in case someone else will try to deploy same version before
            // content.json is ready
            super.storeItem(hiddenRequest, is, userAttributes);
            DefaultStorageFileItem hiddenItem = (DefaultStorageFileItem) super.retrieveItem(request);

            utility.processStoreRequest(hiddenItem, this);
        } catch (ItemNotFoundException e) {
            throw new StorageException(e);
        }
    }

    @SuppressWarnings("deprecation")
    @Override
    public StorageItem retrieveItem(ResourceStoreRequest request) throws IllegalOperationException, ItemNotFoundException, StorageException, AccessDeniedException {
        utility.addNpmMeta(request);
        return super.retrieveItem(request);
    }


}

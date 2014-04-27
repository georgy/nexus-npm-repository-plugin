package com.bolyuba.nexus.plugin.npm.hosted;

import com.bolyuba.nexus.plugin.npm.NpmContentClass;
import com.bolyuba.nexus.plugin.npm.NpmRepository;
import org.codehaus.plexus.util.xml.Xpp3Dom;
import org.sonatype.inject.Description;
import org.sonatype.nexus.configuration.Configurator;
import org.sonatype.nexus.configuration.model.CRepository;
import org.sonatype.nexus.configuration.model.CRepositoryExternalConfigurationHolderFactory;
import org.sonatype.nexus.proxy.AccessDeniedException;
import org.sonatype.nexus.proxy.IllegalOperationException;
import org.sonatype.nexus.proxy.ItemNotFoundException;
import org.sonatype.nexus.proxy.ResourceStoreRequest;
import org.sonatype.nexus.proxy.StorageException;
import org.sonatype.nexus.proxy.item.StorageItem;
import org.sonatype.nexus.proxy.registry.ContentClass;
import org.sonatype.nexus.proxy.repository.AbstractRepository;
import org.sonatype.nexus.proxy.repository.DefaultRepositoryKind;
import org.sonatype.nexus.proxy.repository.RepositoryKind;
import org.sonatype.nexus.proxy.storage.UnsupportedStorageOperationException;

import javax.inject.Inject;
import javax.inject.Named;
import java.io.InputStream;
import java.util.Map;

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

    @Inject
    public DefaultNpmHostedRepository(final @Named(NpmContentClass.ID) ContentClass contentClass,
                                      NpmHostedRepositoryConfigurator configurator) {
        this.contentClass = contentClass;
        this.configurator = configurator;
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
    protected CRepositoryExternalConfigurationHolderFactory<?> getExternalConfigurationHolderFactory() {
        return new CRepositoryExternalConfigurationHolderFactory<NpmHostedRepositoryConfiguration>() {
            @Override
            public NpmHostedRepositoryConfiguration createExternalConfigurationHolder(final CRepository config) {
                return new NpmHostedRepositoryConfiguration((Xpp3Dom) config.getExternalConfiguration());
            }
        };
    }

    @SuppressWarnings("deprecation")
    @Override
    public void storeItem(ResourceStoreRequest request, InputStream is, Map<String, String> userAttributes) throws UnsupportedStorageOperationException, IllegalOperationException, StorageException, AccessDeniedException {
        super.storeItem(request, is, userAttributes);
    }

    @SuppressWarnings("deprecation")
    @Override
    public StorageItem retrieveItem(ResourceStoreRequest request) throws IllegalOperationException, ItemNotFoundException, StorageException, AccessDeniedException {
        return super.retrieveItem(request);
    }
}

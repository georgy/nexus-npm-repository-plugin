package com.bolyuba.nexus.plugin.npm.templates;

import com.bolyuba.nexus.plugin.npm.NpmContentClass;
import com.bolyuba.nexus.plugin.npm.NpmRepository;
import com.bolyuba.nexus.plugin.npm.hosted.DefaultNpmHostedRepository;
import com.bolyuba.nexus.plugin.npm.hosted.NpmHostedRepositoryConfiguration;
import org.codehaus.plexus.util.xml.Xpp3Dom;
import org.sonatype.configuration.ConfigurationException;
import org.sonatype.nexus.configuration.model.CRemoteStorage;
import org.sonatype.nexus.configuration.model.CRepository;
import org.sonatype.nexus.configuration.model.CRepositoryCoreConfiguration;
import org.sonatype.nexus.configuration.model.CRepositoryExternalConfigurationHolderFactory;
import org.sonatype.nexus.configuration.model.DefaultCRepository;
import org.sonatype.nexus.proxy.repository.RepositoryWritePolicy;
import org.sonatype.nexus.templates.repository.AbstractRepositoryTemplate;

import java.io.IOException;

/**
 * @author Georgy Bolyuba (georgy@bolyuba.com)
 */
public class NpmHostedRepositoryTemplate
        extends AbstractRepositoryTemplate
{

    public NpmHostedRepositoryTemplate(final NpmRepositoryTemplateProvider provider, final String id,
                                       final String description) {
        super(provider, id, description, new NpmContentClass(), DefaultNpmHostedRepository.class);
    }

    @Override
    protected CRepositoryCoreConfiguration initCoreConfiguration() {
        final CRepository repo = new DefaultCRepository();
        repo.setId("test");
        repo.setName("test");

        repo.setProviderRole(NpmRepository.class.getName());
        repo.setProviderHint(DefaultNpmHostedRepository.ROLE_HINT);

        repo.setRemoteStorage(new CRemoteStorage());
        repo.getRemoteStorage().setProvider(getTemplateProvider().getRemoteProviderHintFactory().getDefaultHttpRoleHint());
        repo.getRemoteStorage().setUrl("http://some-remote-repository/repo-root/obr.xml");

        final Xpp3Dom ex = new Xpp3Dom(DefaultCRepository.EXTERNAL_CONFIGURATION_NODE_NAME);
        repo.setExternalConfiguration(ex);

        final NpmHostedRepositoryConfiguration exConf = new NpmHostedRepositoryConfiguration(ex);

        repo.externalConfigurationImple = exConf;

        repo.setWritePolicy(RepositoryWritePolicy.ALLOW_WRITE.name());
        repo.setNotFoundCacheTTL(1440);
        repo.setIndexable(true);
        repo.setSearchable(true);

        return new CRepositoryCoreConfiguration(getTemplateProvider().getApplicationConfiguration(), repo,
                new CRepositoryExternalConfigurationHolderFactory<NpmHostedRepositoryConfiguration>() {
                    @Override
                    public NpmHostedRepositoryConfiguration createExternalConfigurationHolder(final CRepository config) {
                        return new NpmHostedRepositoryConfiguration((Xpp3Dom) config.getExternalConfiguration());
                    }
                });
    }

    @Override
    public NpmRepository create() throws ConfigurationException, IOException {
        return (NpmRepository) super.create();
    }
}

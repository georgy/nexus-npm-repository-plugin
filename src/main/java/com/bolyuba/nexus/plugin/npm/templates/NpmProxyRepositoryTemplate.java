package com.bolyuba.nexus.plugin.npm.templates;

import com.bolyuba.nexus.plugin.npm.NpmContentClass;
import com.bolyuba.nexus.plugin.npm.NpmPlugin;
import com.bolyuba.nexus.plugin.npm.NpmRepository;
import com.bolyuba.nexus.plugin.npm.proxy.DefaultNpmProxyRepository;
import com.bolyuba.nexus.plugin.npm.proxy.NpmProxyRepositoryConfiguration;
import org.codehaus.plexus.util.xml.Xpp3Dom;
import org.sonatype.configuration.ConfigurationException;
import org.sonatype.nexus.configuration.model.CRemoteStorage;
import org.sonatype.nexus.configuration.model.CRepository;
import org.sonatype.nexus.configuration.model.CRepositoryCoreConfiguration;
import org.sonatype.nexus.configuration.model.CRepositoryExternalConfigurationHolderFactory;
import org.sonatype.nexus.configuration.model.DefaultCRepository;
import org.sonatype.nexus.proxy.maven.MavenRepository;
import org.sonatype.nexus.proxy.maven.RepositoryPolicy;
import org.sonatype.nexus.proxy.repository.RepositoryWritePolicy;
import org.sonatype.nexus.templates.repository.maven.AbstractMavenRepositoryTemplate;

import java.io.IOException;

/**
 * @author Georgy Bolyuba (georgy@bolyuba.com)
 */
public class NpmProxyRepositoryTemplate
        extends AbstractMavenRepositoryTemplate {

    public NpmProxyRepositoryTemplate(final NpmRepositoryTemplateProvider provider, final String id,
                                      final String description) {
        super(provider, id, description, new NpmContentClass(), DefaultNpmProxyRepository.class, RepositoryPolicy.RELEASE);
    }

    @Override
    protected CRepositoryCoreConfiguration initCoreConfiguration() {
        final CRepository repo = new DefaultCRepository();
        repo.setId("test");
        repo.setName("test");

        repo.setProviderRole(NpmRepository.class.getName());
        repo.setProviderHint(NpmPlugin.ROLE_HINT);

        repo.setRemoteStorage(new CRemoteStorage());
        repo.getRemoteStorage().setProvider(getTemplateProvider().getRemoteProviderHintFactory().getDefaultHttpRoleHint());
        repo.getRemoteStorage().setUrl("http://some-remote-repository/repo-root/obr.xml");
//        repo.getRemoteStorage().setUrl("https://registry.npmjs.org");

        final Xpp3Dom ex = new Xpp3Dom(DefaultCRepository.EXTERNAL_CONFIGURATION_NODE_NAME);
        repo.setExternalConfiguration(ex);

        repo.externalConfigurationImple = new NpmProxyRepositoryConfiguration(ex);

        repo.setWritePolicy(RepositoryWritePolicy.READ_ONLY.name());
        repo.setNotFoundCacheActive(true);
        repo.setNotFoundCacheTTL(1440);

        return new CRepositoryCoreConfiguration(getTemplateProvider().getApplicationConfiguration(), repo,
                new CRepositoryExternalConfigurationHolderFactory<NpmProxyRepositoryConfiguration>() {
                    @Override
                    public NpmProxyRepositoryConfiguration createExternalConfigurationHolder(final CRepository config) {
                        return new NpmProxyRepositoryConfiguration((Xpp3Dom) config.getExternalConfiguration());
                    }
                });
    }

    @Override
    public MavenRepository create() throws ConfigurationException, IOException {
        return super.create();
    }
}

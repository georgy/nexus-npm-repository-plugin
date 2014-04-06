package com.bolyuba.nexus.plugin.npm.templates;

import com.bolyuba.nexus.plugin.npm.proxy.DefaultNpmProxyRepository;
import com.bolyuba.nexus.plugin.npm.NpmContentClass;
import com.bolyuba.nexus.plugin.npm.proxy.NpmProxyRepositoryConfiguration;
import org.codehaus.plexus.util.xml.Xpp3Dom;
import org.sonatype.nexus.configuration.model.*;
import org.sonatype.nexus.proxy.repository.Repository;
import org.sonatype.nexus.proxy.repository.RepositoryWritePolicy;
import org.sonatype.nexus.templates.repository.AbstractRepositoryTemplate;

/**
 * @author Georgy Bolyuba (georgy@bolyuba.com)
 */
public class NpmProxyRepositoryTemplate extends AbstractRepositoryTemplate {

    public NpmProxyRepositoryTemplate(final NpmRepositoryTemplateProvider provider, final String id,
                                      final String description) {
        super(provider, id, description, new NpmContentClass(), DefaultNpmProxyRepository.class);
    }

    public NpmProxyRepositoryConfiguration getExternalConfiguration(final boolean forWrite) {
      return (NpmProxyRepositoryConfiguration) getCoreConfiguration().getExternalConfiguration().getConfiguration(
          forWrite);
    }

    @Override
    protected CRepositoryCoreConfiguration initCoreConfiguration() {
        final CRepository repo = new DefaultCRepository();
        repo.setId("");
        repo.setName("");

        repo.setProviderRole(Repository.class.getName());
        repo.setProviderHint(DefaultNpmProxyRepository.ROLE_HINT);

        repo.setRemoteStorage(new CRemoteStorage());
        repo.getRemoteStorage().setProvider(getTemplateProvider().getRemoteProviderHintFactory().getDefaultHttpRoleHint());
        repo.getRemoteStorage().setUrl("http://some-remote-repository/repo-root/obr.xml");
//        repo.getRemoteStorage().setUrl("https://registry.npmjs.org");

        final Xpp3Dom ex = new Xpp3Dom(DefaultCRepository.EXTERNAL_CONFIGURATION_NODE_NAME);
        repo.setExternalConfiguration(ex);

        final NpmProxyRepositoryConfiguration exConf = new NpmProxyRepositoryConfiguration(ex);

        repo.externalConfigurationImple = exConf;

        repo.setWritePolicy(RepositoryWritePolicy.READ_ONLY.name());
        repo.setNotFoundCacheActive(true);
        repo.setNotFoundCacheTTL(1440);

        final CRepositoryCoreConfiguration result =
                new CRepositoryCoreConfiguration(getTemplateProvider().getApplicationConfiguration(), repo,
                        new CRepositoryExternalConfigurationHolderFactory<NpmProxyRepositoryConfiguration>() {
                            @Override
                            public NpmProxyRepositoryConfiguration createExternalConfigurationHolder(final CRepository config) {
                                return new NpmProxyRepositoryConfiguration((Xpp3Dom) config.getExternalConfiguration());
                            }
                        });

        return result;
    }
}

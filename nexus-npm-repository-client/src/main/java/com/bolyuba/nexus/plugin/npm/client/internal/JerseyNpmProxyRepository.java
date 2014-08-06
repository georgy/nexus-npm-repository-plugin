package com.bolyuba.nexus.plugin.npm.client.internal;

import org.sonatype.nexus.client.internal.rest.jersey.subsystem.repository.JerseyProxyRepository;
import org.sonatype.nexus.client.rest.jersey.JerseyNexusClient;
import org.sonatype.nexus.rest.model.RepositoryProxyResource;

import com.bolyuba.nexus.plugin.npm.client.NpmProxyRepository;

public class JerseyNpmProxyRepository
    extends JerseyProxyRepository<NpmProxyRepository>
    implements NpmProxyRepository
{
  static final String PROVIDER_ROLE = "org.sonatype.nexus.proxy.repository.Repository";

  static final String PROVIDER = "npm-proxy";

  public JerseyNpmProxyRepository(final JerseyNexusClient nexusClient, final String id) {
    super(nexusClient, id);
  }

  public JerseyNpmProxyRepository(final JerseyNexusClient nexusClient,
                                  final RepositoryProxyResource settings)
  {
    super(nexusClient, settings);
  }

  @Override
  protected RepositoryProxyResource createSettings() {
    final RepositoryProxyResource settings = super.createSettings();

    settings.setProviderRole(JerseyNpmProxyRepository.PROVIDER_ROLE);
    settings.setProvider(JerseyNpmProxyRepository.PROVIDER);
    settings.setRepoPolicy("RELEASE");
    settings.setIndexable(false);

    return settings;
  }
}

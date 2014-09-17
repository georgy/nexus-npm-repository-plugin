package com.bolyuba.nexus.plugin.npm.client.internal;

import org.sonatype.nexus.client.internal.rest.jersey.subsystem.repository.JerseyHostedRepository;
import org.sonatype.nexus.client.rest.jersey.JerseyNexusClient;
import org.sonatype.nexus.rest.model.RepositoryResource;

import com.bolyuba.nexus.plugin.npm.client.NpmHostedRepository;

public class JerseyNpmHostedRepository
    extends JerseyHostedRepository<NpmHostedRepository>
    implements NpmHostedRepository
{
  static final String PROVIDER_ROLE = "org.sonatype.nexus.proxy.repository.Repository";

  static final String PROVIDER = "npm-hosted";

  public JerseyNpmHostedRepository(final JerseyNexusClient nexusClient, final String id) {
    super(nexusClient, id);
  }

  public JerseyNpmHostedRepository(final JerseyNexusClient nexusClient,
                                   final RepositoryResource settings)
  {
    super(nexusClient, settings);
  }

  @Override
  protected RepositoryResource createSettings() {
    final RepositoryResource settings = super.createSettings();

    settings.setProviderRole(JerseyNpmHostedRepository.PROVIDER_ROLE);
    settings.setProvider(JerseyNpmHostedRepository.PROVIDER);
    settings.setRepoPolicy("RELEASE");
    settings.setIndexable(false);

    return settings;
  }
}

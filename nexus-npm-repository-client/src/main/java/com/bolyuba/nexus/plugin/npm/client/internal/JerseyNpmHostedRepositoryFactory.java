package com.bolyuba.nexus.plugin.npm.client.internal;

import javax.inject.Named;
import javax.inject.Singleton;

import org.sonatype.nexus.client.core.subsystem.repository.Repository;
import org.sonatype.nexus.client.internal.rest.jersey.subsystem.repository.JerseyHostedRepositoryFactory;
import org.sonatype.nexus.client.rest.jersey.JerseyNexusClient;
import org.sonatype.nexus.rest.model.RepositoryBaseResource;
import org.sonatype.nexus.rest.model.RepositoryResource;

import com.bolyuba.nexus.plugin.npm.client.NpmHostedRepository;

@Named
@Singleton
public class JerseyNpmHostedRepositoryFactory
    extends JerseyHostedRepositoryFactory
{
  @Override
  public int canAdapt(final RepositoryBaseResource resource) {
    int score = super.canAdapt(resource);
    if (score > 0) {
      if (JerseyNpmHostedRepository.PROVIDER_ROLE.equals(resource.getProviderRole()) &&
          JerseyNpmHostedRepository.PROVIDER.equals(resource.getProvider())) {
        score++;
      }
    }
    return score;
  }

  @Override
  public JerseyNpmHostedRepository adapt(final JerseyNexusClient nexusClient,
                                         final RepositoryBaseResource resource)
  {
    return new JerseyNpmHostedRepository(nexusClient, (RepositoryResource) resource);
  }

  @Override
  public boolean canCreate(final Class<? extends Repository> type) {
    return NpmHostedRepository.class.equals(type);
  }

  @Override
  public JerseyNpmHostedRepository create(final JerseyNexusClient nexusClient, final String id) {
    return new JerseyNpmHostedRepository(nexusClient, id);
  }

}

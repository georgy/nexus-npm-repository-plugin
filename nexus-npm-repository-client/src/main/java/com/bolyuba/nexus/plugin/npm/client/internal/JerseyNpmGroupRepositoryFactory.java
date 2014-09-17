package com.bolyuba.nexus.plugin.npm.client.internal;

import javax.inject.Named;
import javax.inject.Singleton;

import org.sonatype.nexus.client.core.subsystem.repository.Repository;
import org.sonatype.nexus.client.internal.rest.jersey.subsystem.repository.JerseyGroupRepositoryFactory;
import org.sonatype.nexus.client.rest.jersey.JerseyNexusClient;
import org.sonatype.nexus.rest.model.RepositoryBaseResource;
import org.sonatype.nexus.rest.model.RepositoryGroupResource;

import com.bolyuba.nexus.plugin.npm.client.NpmGroupRepository;

@Named
@Singleton
public class JerseyNpmGroupRepositoryFactory
    extends JerseyGroupRepositoryFactory
{
  @Override
  public int canAdapt(final RepositoryBaseResource resource) {
    int score = super.canAdapt(resource);
    if (score > 0) {
      if (JerseyNpmGroupRepository.PROVIDER_ROLE.equals(resource.getProviderRole()) &&
          JerseyNpmGroupRepository.PROVIDER.equals(resource.getProvider())) {
        score++;
      }
    }
    return score;
  }

  @Override
  public JerseyNpmGroupRepository adapt(final JerseyNexusClient nexusClient,
                                        final RepositoryBaseResource resource)
  {
    return new JerseyNpmGroupRepository(nexusClient, (RepositoryGroupResource) resource);
  }

  @Override
  public boolean canCreate(final Class<? extends Repository> type) {
    return NpmGroupRepository.class.equals(type);
  }

  @Override
  public JerseyNpmGroupRepository create(final JerseyNexusClient nexusClient, final String id) {
    return new JerseyNpmGroupRepository(nexusClient, id);
  }

}

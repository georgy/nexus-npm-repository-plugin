package com.bolyuba.nexus.plugin.npm.client.internal;

import javax.inject.Named;
import javax.inject.Singleton;

import org.sonatype.nexus.client.core.subsystem.repository.Repository;
import org.sonatype.nexus.client.internal.rest.jersey.subsystem.repository.JerseyProxyRepositoryFactory;
import org.sonatype.nexus.client.rest.jersey.JerseyNexusClient;
import org.sonatype.nexus.rest.model.RepositoryBaseResource;
import org.sonatype.nexus.rest.model.RepositoryProxyResource;

import com.bolyuba.nexus.plugin.npm.client.NpmProxyRepository;

@Named
@Singleton
public class JerseyNpmProxyRepositoryFactory
    extends JerseyProxyRepositoryFactory
{
  @Override
  public int canAdapt(final RepositoryBaseResource resource) {
    int score = super.canAdapt(resource);
    if (score > 0) {
      if (JerseyNpmProxyRepository.PROVIDER.equals(resource.getProvider())) {
        score++;
      }
    }
    return score;
  }

  @Override
  public JerseyNpmProxyRepository adapt(final JerseyNexusClient nexusClient,
                                        final RepositoryBaseResource resource)
  {
    return new JerseyNpmProxyRepository(nexusClient, (RepositoryProxyResource) resource);
  }

  @Override
  public boolean canCreate(final Class<? extends Repository> type) {
    return NpmProxyRepository.class.equals(type);
  }

  @Override
  public JerseyNpmProxyRepository create(final JerseyNexusClient nexusClient, final String id) {
    return new JerseyNpmProxyRepository(nexusClient, id);
  }

}

package com.bolyuba.nexus.plugin.npm.metadata.internal;

import javax.inject.Inject;
import javax.inject.Named;
import javax.inject.Singleton;

import org.sonatype.nexus.configuration.application.ApplicationDirectories;
import org.sonatype.nexus.proxy.storage.remote.httpclient.HttpClientManager;
import org.sonatype.sisu.goodies.common.ComponentSupport;

import com.bolyuba.nexus.plugin.npm.NpmRepository;
import com.bolyuba.nexus.plugin.npm.group.NpmGroupRepository;
import com.bolyuba.nexus.plugin.npm.hosted.NpmHostedRepository;
import com.bolyuba.nexus.plugin.npm.metadata.GroupMetadataService;
import com.bolyuba.nexus.plugin.npm.metadata.HostedMetadataService;
import com.bolyuba.nexus.plugin.npm.metadata.MetadataServiceFactory;
import com.bolyuba.nexus.plugin.npm.metadata.ProxyMetadataService;
import com.bolyuba.nexus.plugin.npm.proxy.NpmProxyRepository;

import static com.google.common.base.Preconditions.checkNotNull;

/**
 * {@link MetadataServiceFactory} implementation.
 */
@Singleton
@Named
public class MetadataServiceFactoryImpl
    extends ComponentSupport
    implements MetadataServiceFactory
{
  private final MetadataStore metadataStore;

  private final HttpClientManager httpClientManager;

  private final MetadataParser metadataParser;

  @Inject
  public MetadataServiceFactoryImpl(final ApplicationDirectories applicationDirectories,
                                    final MetadataStore metadataStore,
                                    final HttpClientManager httpClientManager)
  {
    this.metadataStore = checkNotNull(metadataStore);
    this.httpClientManager = checkNotNull(httpClientManager);
    this.metadataParser = new MetadataParser(applicationDirectories.getTemporaryDirectory());
  }

  private MetadataGenerator createGenerator(final NpmRepository npmRepository) {
    return new MetadataGenerator(npmRepository, metadataStore);
  }

  @Override
  public HostedMetadataService createHostedMetadataService(final NpmHostedRepository npmHostedRepository) {
    return new HostedMetadataServiceImpl(npmHostedRepository, createGenerator(npmHostedRepository), metadataParser);
  }

  @Override
  public ProxyMetadataService createProxyMetadataService(final NpmProxyRepository npmProxyRepository) {
    return new ProxyMetadataServiceImpl(npmProxyRepository, httpClientManager, metadataStore,
        createGenerator(npmProxyRepository), metadataParser);
  }

  @Override
  public GroupMetadataService createGroupMetadataService(final NpmGroupRepository npmGroupRepository) {
    throw new UnsupportedOperationException("not yet there");
  }
}

package com.bolyuba.nexus.plugin.npm.metadata.internal;

import javax.inject.Inject;
import javax.inject.Named;
import javax.inject.Singleton;

import org.sonatype.nexus.configuration.application.ApplicationDirectories;
import org.sonatype.nexus.proxy.storage.remote.httpclient.HttpClientManager;
import org.sonatype.sisu.goodies.common.ComponentSupport;

import com.bolyuba.nexus.plugin.npm.NpmRepository;
import com.bolyuba.nexus.plugin.npm.hosted.NpmHostedRepository;
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
  private final ApplicationDirectories applicationDirectories;

  private final MetadataStore metadataStore;

  private final HttpClientManager httpClientManager;

  @Inject
  public MetadataServiceFactoryImpl(final ApplicationDirectories applicationDirectories,
                                    final MetadataStore metadataStore, final HttpClientManager httpClientManager)
  {
    this.applicationDirectories = checkNotNull(applicationDirectories);
    this.metadataStore = checkNotNull(metadataStore);
    this.httpClientManager = checkNotNull(httpClientManager);
  }

  private MetadataParser createParser(final NpmRepository npmRepository) {
    return new MetadataParser(applicationDirectories.getTemporaryDirectory(), npmRepository);
  }

  private MetadataConsumer createConsumer(final NpmRepository npmRepository) {
    return new MetadataConsumer(npmRepository, createParser(npmRepository), metadataStore);
  }

  private MetadataProducer createProducer(final NpmRepository npmRepository) {
    return new MetadataProducer(npmRepository, metadataStore);
  }

  @Override
  public HostedMetadataService createHostedMetadataService(final NpmHostedRepository npmHostedRepository) {
    return new HostedMetadataServiceImpl(createConsumer(npmHostedRepository), createProducer(npmHostedRepository));
  }

  @Override
  public ProxyMetadataService createProxyMetadataService(final NpmProxyRepository npmProxyRepository) {
    return new ProxyMetadataServiceImpl(npmProxyRepository, httpClientManager, metadataStore,
        createParser(npmProxyRepository), createProducer(npmProxyRepository));
  }
}

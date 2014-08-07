package com.bolyuba.nexus.plugin.npm.metadata.internal;

import javax.inject.Inject;
import javax.inject.Named;
import javax.inject.Singleton;

import org.sonatype.sisu.goodies.common.ComponentSupport;

import com.bolyuba.nexus.plugin.npm.NpmRepository;
import com.bolyuba.nexus.plugin.npm.group.NpmGroupRepository;
import com.bolyuba.nexus.plugin.npm.hosted.NpmHostedRepository;
import com.bolyuba.nexus.plugin.npm.metadata.GroupMetadataService;
import com.bolyuba.nexus.plugin.npm.metadata.HostedMetadataService;
import com.bolyuba.nexus.plugin.npm.metadata.MetadataServiceFactory;
import com.bolyuba.nexus.plugin.npm.metadata.ProxyMetadataService;
import com.bolyuba.nexus.plugin.npm.proxy.NpmProxyRepository;
import com.google.common.annotations.VisibleForTesting;

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

  private final MetadataParser metadataParser;

  private final ProxyMetadataTransport httpProxyMetadataTransport;

  @Inject
  public MetadataServiceFactoryImpl(final MetadataStore metadataStore,
                                    final MetadataParser metadataParser,
                                    final ProxyMetadataTransport httpProxyMetadataTransport)
  {
    this.metadataStore = checkNotNull(metadataStore);
    this.metadataParser = checkNotNull(metadataParser);
    this.httpProxyMetadataTransport = checkNotNull(httpProxyMetadataTransport);
  }

  @VisibleForTesting
  public MetadataParser getMetadataParser() {
    return metadataParser;
  }

  @VisibleForTesting
  public ProxyMetadataTransport getProxyMetadataTransport() {
    return httpProxyMetadataTransport;
  }

  @VisibleForTesting
  public MetadataGenerator createGenerator(final NpmRepository npmRepository) {
    return new MetadataGenerator(npmRepository, metadataStore);
  }

  @Override
  public HostedMetadataService createHostedMetadataService(final NpmHostedRepository npmHostedRepository) {
    return new HostedMetadataServiceImpl(npmHostedRepository, createGenerator(npmHostedRepository), metadataParser);
  }

  @Override
  public ProxyMetadataService createProxyMetadataService(final NpmProxyRepository npmProxyRepository) {
    return new ProxyMetadataServiceImpl(npmProxyRepository, metadataStore,
        createGenerator(npmProxyRepository), getProxyMetadataTransport(), metadataParser);
  }

  @Override
  public GroupMetadataService createGroupMetadataService(final NpmGroupRepository npmGroupRepository) {
    return new GroupMetadataServiceImpl(npmGroupRepository, metadataParser);
  }
}

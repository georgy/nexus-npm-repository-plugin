package com.bolyuba.nexus.plugin.npm.service.internal;

import javax.inject.Inject;
import javax.inject.Named;
import javax.inject.Singleton;

import org.sonatype.sisu.goodies.common.ComponentSupport;

import com.bolyuba.nexus.plugin.npm.group.NpmGroupRepository;
import com.bolyuba.nexus.plugin.npm.hosted.NpmHostedRepository;
import com.bolyuba.nexus.plugin.npm.proxy.NpmProxyRepository;
import com.bolyuba.nexus.plugin.npm.service.GroupMetadataService;
import com.bolyuba.nexus.plugin.npm.service.HostedMetadataService;
import com.bolyuba.nexus.plugin.npm.service.MetadataServiceFactory;
import com.bolyuba.nexus.plugin.npm.service.ProxyMetadataService;
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

  private final ProxyMetadataTransport proxyMetadataTransport;

  @Inject
  public MetadataServiceFactoryImpl(final MetadataStore metadataStore,
                                    final MetadataParser metadataParser,
                                    final ProxyMetadataTransport proxyMetadataTransport)
  {
    this.metadataStore = checkNotNull(metadataStore);
    this.metadataParser = checkNotNull(metadataParser);
    this.proxyMetadataTransport = checkNotNull(proxyMetadataTransport);
  }

  @VisibleForTesting
  public ProxyMetadataTransport getProxyMetadataTransport() {
    return proxyMetadataTransport;
  }

  @Override
  public HostedMetadataService createHostedMetadataService(final NpmHostedRepository npmHostedRepository) {
    return new HostedMetadataServiceImpl(npmHostedRepository, metadataStore, metadataParser);
  }

  @Override
  public ProxyMetadataService createProxyMetadataService(final NpmProxyRepository npmProxyRepository) {
    return new ProxyMetadataServiceImpl(npmProxyRepository, metadataStore,
        getProxyMetadataTransport(), metadataParser);
  }

  @Override
  public GroupMetadataService createGroupMetadataService(final NpmGroupRepository npmGroupRepository) {
    return new GroupMetadataServiceImpl(npmGroupRepository, metadataParser);
  }
}

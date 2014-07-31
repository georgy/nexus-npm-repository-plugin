package com.bolyuba.nexus.plugin.npm.metadata.internal;

import javax.inject.Inject;
import javax.inject.Named;
import javax.inject.Singleton;

import org.sonatype.sisu.goodies.common.ComponentSupport;

import com.bolyuba.nexus.plugin.npm.NpmRepository;
import com.bolyuba.nexus.plugin.npm.metadata.MetadataConsumer;
import com.bolyuba.nexus.plugin.npm.metadata.MetadataProducer;
import com.bolyuba.nexus.plugin.npm.metadata.MetadataService;
import com.bolyuba.nexus.plugin.npm.metadata.MetadataStore;

import static com.google.common.base.Preconditions.checkNotNull;

/**
 * Created by cstamas on 29/07/14.
 */
@Singleton
@Named
public class MetadataServiceImpl
    extends ComponentSupport
    implements MetadataService
{
  private final MetadataStore metadataStore;

  @Inject
  public MetadataServiceImpl(final MetadataStore metadataStore) {
    this.metadataStore = checkNotNull(metadataStore);
  }

  @Override
  public MetadataStore getStore() {
    return metadataStore;
  }

  @Override
  public MetadataConsumer createConsumer(final NpmRepository npmRepository) {
    return new MetadataConsumerImpl(npmRepository, metadataStore);
  }

  @Override
  public MetadataProducer createProducer(final NpmRepository npmRepository) {
    return new MetadataProducerImpl(npmRepository, metadataStore);
  }
}

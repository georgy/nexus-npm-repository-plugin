package com.bolyuba.nexus.plugin.npm.metadata;

import com.bolyuba.nexus.plugin.npm.NpmRepository;

public interface MetadataService
{
  MetadataStore getStore();

  MetadataConsumer createConsumer(NpmRepository npmRepository);

  MetadataProducer createProducer(NpmRepository npmRepository);
}

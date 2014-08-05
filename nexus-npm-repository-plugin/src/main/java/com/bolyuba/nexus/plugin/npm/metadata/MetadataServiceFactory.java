package com.bolyuba.nexus.plugin.npm.metadata;

import com.bolyuba.nexus.plugin.npm.group.NpmGroupRepository;
import com.bolyuba.nexus.plugin.npm.hosted.NpmHostedRepository;
import com.bolyuba.nexus.plugin.npm.proxy.NpmProxyRepository;

public interface MetadataServiceFactory
{
  HostedMetadataService createHostedMetadataService(NpmHostedRepository npmHostedRepository);

  ProxyMetadataService createProxyMetadataService(NpmProxyRepository npmProxyRepository);

  GroupMetadataService createGroupMetadataService(NpmGroupRepository npmGroupRepository);

  Producer createProducerFromGenerator(Generator generator);
}

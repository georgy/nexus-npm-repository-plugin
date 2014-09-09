package com.bolyuba.nexus.plugin.npm.hosted;

import org.sonatype.nexus.proxy.repository.HostedRepository;

import com.bolyuba.nexus.plugin.npm.NpmRepository;
import com.bolyuba.nexus.plugin.npm.service.HostedMetadataService;

/**
 * @author Georgy Bolyuba (georgy@bolyuba.com)
 */
public interface NpmHostedRepository
    extends NpmRepository, HostedRepository
{
  @Override
  HostedMetadataService getMetadataService();
}

package com.bolyuba.nexus.plugin.npm.hosted;

import com.bolyuba.nexus.plugin.npm.NpmRepository;
import com.bolyuba.nexus.plugin.npm.metadata.HostedMetadataService;

import org.sonatype.nexus.proxy.repository.HostedRepository;

/**
 * @author Georgy Bolyuba (georgy@bolyuba.com)
 */
public interface NpmHostedRepository
        extends NpmRepository, HostedRepository {

  HostedMetadataService getMetadataService();
}

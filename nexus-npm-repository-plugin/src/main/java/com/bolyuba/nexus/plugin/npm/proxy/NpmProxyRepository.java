package com.bolyuba.nexus.plugin.npm.proxy;

import com.bolyuba.nexus.plugin.npm.NpmRepository;
import com.bolyuba.nexus.plugin.npm.service.ProxyMetadataService;

import org.sonatype.nexus.proxy.repository.ProxyRepository;

/**
 * A full proxy of Npm registry (for example, https://registry.npmjs.org/).
 *
 * @author Georgy Bolyuba (georgy@bolyuba.com)
 */
public interface NpmProxyRepository
        extends NpmRepository, ProxyRepository {

  @Override
  ProxyMetadataService getMetadataService();

}

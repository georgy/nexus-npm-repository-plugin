package com.bolyuba.nexus.plugin.npm.proxy;

import com.bolyuba.nexus.plugin.npm.NpmRepository;
import org.sonatype.nexus.proxy.repository.ProxyRepository;

/**
 * A full proxy of Npm registry (for example, https://registry.npmjs.org/).
 *
 * @author Georgy Bolyuba (georgy@bolyuba.com)
 */
public interface NpmProxyRepository
        extends NpmRepository, ProxyRepository {
}

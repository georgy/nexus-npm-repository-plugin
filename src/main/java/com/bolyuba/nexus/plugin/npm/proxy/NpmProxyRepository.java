package com.bolyuba.nexus.plugin.npm.proxy;

import org.sonatype.nexus.plugins.RepositoryType;
import org.sonatype.nexus.proxy.repository.ProxyRepository;

/**
 * A full proxy of Npm registry (for example, https://registry.npmjs.org/).
 *
 * @author Georgy Bolyuba (georgy@bolyuba.com)
 */
@RepositoryType(pathPrefix = "npm")
public interface NpmProxyRepository
        extends ProxyRepository {
}

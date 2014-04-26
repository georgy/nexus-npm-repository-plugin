package com.bolyuba.nexus.plugin.npm.hosted;

import org.sonatype.nexus.plugins.RepositoryType;
import org.sonatype.nexus.proxy.repository.HostedRepository;

/**
 * @author Georgy Bolyuba (georgy@bolyuba.com)
 */
@SuppressWarnings("deprecation")
@RepositoryType(pathPrefix = "npm")
public interface NpmHostedRepository
        extends HostedRepository {
}

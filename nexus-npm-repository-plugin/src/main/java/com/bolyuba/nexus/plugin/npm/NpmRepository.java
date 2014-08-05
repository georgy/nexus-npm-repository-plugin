package com.bolyuba.nexus.plugin.npm;

import org.sonatype.nexus.plugins.RepositoryType;
import org.sonatype.nexus.proxy.repository.Repository;

/**
 * @author Georgy Bolyuba (georgy@bolyuba.com)
 */
@SuppressWarnings("deprecation")
@RepositoryType(pathPrefix = "npm")
public interface NpmRepository extends Repository
{
    String JSON_MIME_TYPE = "application/json";

    String TARBALL_MIME_TYPE = "application/x-gzip";

    String NPM_REGISTRY_SPECIAL = "-";
}

package com.bolyuba.nexus.plugin.npm;

import org.sonatype.nexus.plugins.RepositoryType;

/**
 * @author Georgy Bolyuba (georgy@bolyuba.com)
 */
@SuppressWarnings("deprecation")
@RepositoryType(pathPrefix = "npm")
public interface NpmRepository {

    String JSON_CONTENT_FILE_NAME = "content.json";

}

package com.bolyuba.nexus.plugin.npm;

import org.sonatype.nexus.plugins.RepositoryType;

/**
 * @author Georgy Bolyuba (georgy@bolyuba.com)
 */
@SuppressWarnings("deprecation")
@RepositoryType(pathPrefix = "npm")
public interface NpmRepository {

    /**
     * The name of the file starts with - to avoid collisions with commonjs package
     * names and version. For better or worse, "content.json" is a valid version in
     * common.js. Go figure!
     */
    String JSON_CONTENT_FILE_NAME = "-content.json";

}

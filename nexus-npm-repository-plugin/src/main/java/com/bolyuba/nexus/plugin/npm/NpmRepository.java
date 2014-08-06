package com.bolyuba.nexus.plugin.npm;

import org.sonatype.nexus.proxy.repository.Repository;

import com.bolyuba.nexus.plugin.npm.metadata.Generator;

/**
 * @author Georgy Bolyuba (georgy@bolyuba.com)
 */
public interface NpmRepository
    extends Repository
{
  String JSON_MIME_TYPE = "application/json";

  String TARBALL_MIME_TYPE = "application/x-gzip";

  String NPM_REGISTRY_SPECIAL = "-";

  String NPM_METADATA_SERVICED = "NpmMetadataServiced";

  Generator getMetadataService();
}

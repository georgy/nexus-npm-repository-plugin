package com.bolyuba.nexus.plugin.npm;

import org.sonatype.nexus.proxy.repository.Repository;

import com.bolyuba.nexus.plugin.npm.internal.NpmMimeRulesSource;
import com.bolyuba.nexus.plugin.npm.service.Generator;

/**
 * @author Georgy Bolyuba (georgy@bolyuba.com)
 */
public interface NpmRepository
    extends Repository
{
  /**
   * Mime type used for npm metadata downstream. See {@link NpmMimeRulesSource}.
   */
  String JSON_MIME_TYPE = "application/json";

  /**
   * Mime type used for npm tarballs downstream. See {@link NpmMimeRulesSource}.
   */
  String TARBALL_MIME_TYPE = "application/x-gzip";

  /**
   * Registry "escape" character, that is invalid package name or version.
   */
  String NPM_REGISTRY_SPECIAL = "-";

  /**
   * Key for flag used to mark a store request "already serviced" by NPM metadata service.
   */
  String NPM_METADATA_SERVICED = "NpmMetadataServiced";

  /**
   * Key for flag used to disable NPM metadata service in repository.
   */
  String NPM_METADATA_NO_SERVICE = "NpmMetadataNoService";

  /**
   * Returns the npm repository's metadata service. At this level, it's is basically a {@link Generator}, but see
   * specialized interfaces for NPM hosted, proxy and group repositories.
   */
  Generator getMetadataService();
}

package com.bolyuba.nexus.plugin.npm.content;

import org.sonatype.nexus.mime.MimeRulesSource;

import com.bolyuba.nexus.plugin.npm.NpmRepository;

/**
 * @author Georgy Bolyuba (georgy@bolyuba.com)
 */
public class NpmMimeRulesSource
    implements MimeRulesSource
{
  @Override
  public String getRuleForPath(String path) {
    if (path == null) {
      return null;
    }
    if (path.toLowerCase().endsWith(".tgz")) {
      return NpmRepository.TARBALL_MIME_TYPE;
    }
    if (!path.toLowerCase().contains(NpmRepository.NPM_REGISTRY_SPECIAL)) {
      return NpmRepository.JSON_MIME_TYPE;
    }
    return null;
  }
}

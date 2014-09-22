package com.bolyuba.nexus.plugin.npm.internal;

import javax.inject.Named;
import javax.inject.Singleton;

import org.sonatype.security.realms.tools.AbstractStaticSecurityResource;

@Named
@Singleton
public class NpmSecurityResource extends AbstractStaticSecurityResource
{
  @Override
  protected String getResourcePath() {
    return "/META-INF/nexus-npm-repository-plugin-security.xml";
  }
}

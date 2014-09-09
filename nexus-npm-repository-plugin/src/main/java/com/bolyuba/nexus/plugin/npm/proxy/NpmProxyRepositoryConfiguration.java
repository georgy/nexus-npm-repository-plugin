package com.bolyuba.nexus.plugin.npm.proxy;

import org.sonatype.nexus.proxy.repository.AbstractProxyRepositoryConfiguration;

import org.codehaus.plexus.util.xml.Xpp3Dom;

/**
 * @author Georgy Bolyuba (georgy@bolyuba.com)
 */
public class NpmProxyRepositoryConfiguration
    extends AbstractProxyRepositoryConfiguration
{

  public NpmProxyRepositoryConfiguration(Xpp3Dom configuration) {
    super(configuration);
  }
}

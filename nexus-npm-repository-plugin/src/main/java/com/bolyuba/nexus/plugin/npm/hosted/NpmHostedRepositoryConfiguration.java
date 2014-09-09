package com.bolyuba.nexus.plugin.npm.hosted;

import org.sonatype.nexus.proxy.repository.AbstractRepositoryConfiguration;

import org.codehaus.plexus.util.xml.Xpp3Dom;

/**
 * @author Georgy Bolyuba (georgy@bolyuba.com)
 */
public class NpmHostedRepositoryConfiguration
    extends AbstractRepositoryConfiguration
{
  public NpmHostedRepositoryConfiguration(Xpp3Dom configuration) {
    super(configuration);
  }
}

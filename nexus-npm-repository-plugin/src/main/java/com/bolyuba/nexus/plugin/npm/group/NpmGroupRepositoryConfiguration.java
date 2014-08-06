package com.bolyuba.nexus.plugin.npm.group;

import org.sonatype.nexus.proxy.repository.AbstractGroupRepositoryConfiguration;

import org.codehaus.plexus.util.xml.Xpp3Dom;

/**
 * @author Georgy Bolyuba (georgy@bolyuba.com)
 */
public class NpmGroupRepositoryConfiguration
    extends AbstractGroupRepositoryConfiguration
{
  public NpmGroupRepositoryConfiguration(Xpp3Dom configuration) {
    super(configuration);
  }
}

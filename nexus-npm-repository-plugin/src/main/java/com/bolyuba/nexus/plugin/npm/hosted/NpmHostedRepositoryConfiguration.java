package com.bolyuba.nexus.plugin.npm.hosted;

import org.codehaus.plexus.util.xml.Xpp3Dom;
import org.sonatype.nexus.proxy.maven.AbstractMavenRepositoryConfiguration;

/**
 * @author Georgy Bolyuba (georgy@bolyuba.com)
 */
public class NpmHostedRepositoryConfiguration
        extends AbstractMavenRepositoryConfiguration {

    public NpmHostedRepositoryConfiguration(Xpp3Dom configuration) {
        super(configuration);
    }
}

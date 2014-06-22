package com.bolyuba.nexus.plugin.npm.proxy;

import org.codehaus.plexus.util.xml.Xpp3Dom;
import org.sonatype.nexus.proxy.maven.AbstractMavenRepositoryConfiguration;

/**
 * @author Georgy Bolyuba (georgy@bolyuba.com)
 */
public class NpmProxyRepositoryConfiguration
        extends AbstractMavenRepositoryConfiguration {

    public NpmProxyRepositoryConfiguration(Xpp3Dom configuration) {
        super(configuration);
    }

}

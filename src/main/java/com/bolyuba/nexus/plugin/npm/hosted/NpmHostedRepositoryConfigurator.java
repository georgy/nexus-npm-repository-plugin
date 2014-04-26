package com.bolyuba.nexus.plugin.npm.hosted;

import org.sonatype.nexus.proxy.repository.AbstractRepositoryConfigurator;

import javax.inject.Named;
import javax.inject.Singleton;

/**
 * @author Georgy Bolyuba (georgy@bolyuba.com)
 */
@Named
@Singleton
public class NpmHostedRepositoryConfigurator
        extends AbstractRepositoryConfigurator {
}

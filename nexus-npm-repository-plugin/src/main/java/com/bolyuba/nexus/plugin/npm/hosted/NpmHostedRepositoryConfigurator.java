package com.bolyuba.nexus.plugin.npm.hosted;

import javax.inject.Named;
import javax.inject.Singleton;

import org.sonatype.nexus.proxy.repository.AbstractRepositoryConfigurator;

/**
 * @author Georgy Bolyuba (georgy@bolyuba.com)
 */
@Named
@Singleton
public class NpmHostedRepositoryConfigurator
    extends AbstractRepositoryConfigurator
{
}

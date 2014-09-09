package com.bolyuba.nexus.plugin.npm.proxy;

import javax.inject.Named;
import javax.inject.Singleton;

import org.sonatype.nexus.proxy.repository.AbstractProxyRepositoryConfigurator;

/**
 * @author Georgy Bolyuba (georgy@bolyuba.com)
 */
@Named
@Singleton
public class NpmProxyRepositoryConfigurator
    extends AbstractProxyRepositoryConfigurator
{
}

package com.bolyuba.nexus.plugin.npm.proxy;

import org.sonatype.nexus.proxy.repository.AbstractProxyRepositoryConfigurator;

import javax.inject.Named;
import javax.inject.Singleton;

/**
 * @author Georgy Bolyuba (georgy@bolyuba.com)
 */
@Named
@Singleton
public class NpmProxyRepositoryConfigurator
        extends AbstractProxyRepositoryConfigurator {
}

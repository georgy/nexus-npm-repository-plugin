package com.bolyuba.nexus.plugin.npm.group;

import javax.inject.Named;
import javax.inject.Singleton;

import org.sonatype.nexus.proxy.repository.AbstractGroupRepositoryConfigurator;

/**
 * @author Georgy Bolyuba (georgy@bolyuba.com)
 */
@Named
@Singleton
public class NpmGroupRepositoryConfigurator
    extends AbstractGroupRepositoryConfigurator
{
}

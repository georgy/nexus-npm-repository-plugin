package com.bolyuba.nexus.plugin.npm;

import org.sonatype.nexus.proxy.registry.AbstractIdContentClass;

import javax.inject.Named;
import javax.inject.Singleton;

/**
 * @author Georgy Bolyuba (georgy@bolyuba.com)
 */
@Named(NpmContentClass.ID)
@Singleton
public class NpmContentClass
        extends AbstractIdContentClass {

    public static final String ID = "npm";

    public static final String NAME = "NPM";

    @Override
    public String getId() {
        return ID;
    }

    @Override
    public String getName() {
        return NAME;
    }
}

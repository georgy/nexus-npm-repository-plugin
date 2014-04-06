package com.bolyuba.nexus.plugin.npm.ui;

import com.bolyuba.nexus.plugin.npm.NpmPlugin;
import org.sonatype.nexus.plugins.ui.contribution.UiContributorSupport;

import javax.inject.Inject;
import javax.inject.Named;
import javax.inject.Singleton;

/**
 * This tells Nexus that plugin has static resources (js and css)
 *
 * @author Georgy Bolyuba (georgy@bolyuba.com)
 */
@Named
@Singleton
public class NpmUiContributor
        extends UiContributorSupport {

    @Inject
    public NpmUiContributor(NpmPlugin owner) {
        super(owner);
    }

}

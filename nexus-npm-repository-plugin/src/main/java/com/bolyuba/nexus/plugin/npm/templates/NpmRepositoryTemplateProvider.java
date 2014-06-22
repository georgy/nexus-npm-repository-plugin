package com.bolyuba.nexus.plugin.npm.templates;

import org.sonatype.nexus.templates.TemplateSet;
import org.sonatype.nexus.templates.repository.AbstractRepositoryTemplateProvider;

import javax.inject.Named;
import javax.inject.Singleton;

/**
 * @author Georgy Bolyuba (georgy@bolyuba.com)
 */
@Named(NpmRepositoryTemplateProvider.PROVIDER_ID)
@Singleton
public class NpmRepositoryTemplateProvider extends AbstractRepositoryTemplateProvider {

    public static final String PROVIDER_ID = "npm-repository";

    private static final String NPM_PROXY = "npm_proxy";

    private static final String NPM_HOSTED = "npm_hosted";

    public static final String NPM_PROVIDER = "Npm plugin";

    @Override
    public TemplateSet getTemplates() {
        final TemplateSet templates = new TemplateSet(null);

        templates.add(new NpmProxyRepositoryTemplate(this, NPM_PROXY, NPM_PROVIDER));
        templates.add(new NpmHostedRepositoryTemplate(this, NPM_HOSTED, NPM_PROVIDER));

        return templates;
    }
}

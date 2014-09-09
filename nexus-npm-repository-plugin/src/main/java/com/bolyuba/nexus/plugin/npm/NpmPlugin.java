package com.bolyuba.nexus.plugin.npm;

import javax.inject.Named;
import javax.inject.Singleton;

import org.sonatype.nexus.plugin.PluginIdentity;

/**
 * This plugin adds Npm support to Sonatype Nexus. Implementation is based on
 * http://wiki.commonjs.org/wiki/Packages/Registry spec and behaviour of https://registry.npmjs.org
 *
 * @author Georgy Bolyuba (georgy@bolyuba.com)
 */
@Named
@Singleton
public class NpmPlugin
    extends PluginIdentity
{

  public static final String GROUP_ID = "org.sonatype.nexus.plugins";

  public static final String ARTIFACT_ID = "nexus-npm-repository-plugin";

  public NpmPlugin() throws Exception {
    super(GROUP_ID, ARTIFACT_ID);
  }
}

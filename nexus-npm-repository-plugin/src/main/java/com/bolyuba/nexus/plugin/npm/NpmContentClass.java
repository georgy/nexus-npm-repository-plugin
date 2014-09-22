package com.bolyuba.nexus.plugin.npm;

import javax.inject.Named;
import javax.inject.Singleton;

import org.sonatype.nexus.proxy.registry.AbstractIdContentClass;

/**
 * This class drives:
 * - read-only value for field called "Format" on New repo pages
 * Details:
 * = REST: service/local/components/repo_types?repoType=proxy
 * = Class RepositoryTypesComponentListPlexusResource
 *
 * - item in Repository Targets list. A default "All (npm) .*" target will be created on startup if missing
 * Details:
 * = REST: service/local/repo_targets
 * = Event listener class DefaultTargetRegistryEventInspector adds repo target into nexus config on startup
 *
 * @author Georgy Bolyuba (georgy@bolyuba.com)
 */
@Named(NpmContentClass.ID)
@Singleton
public class NpmContentClass
    extends AbstractIdContentClass
{

  public static final String ID = "npm";

  @Override
  public String getId() {
    return ID;
  }

  @Override
  public String getName() {
    return ID;
  }
}

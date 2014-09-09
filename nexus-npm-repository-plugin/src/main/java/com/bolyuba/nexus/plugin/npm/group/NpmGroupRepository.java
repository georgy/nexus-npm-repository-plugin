package com.bolyuba.nexus.plugin.npm.group;

import org.sonatype.nexus.proxy.repository.GroupRepository;

import com.bolyuba.nexus.plugin.npm.NpmRepository;
import com.bolyuba.nexus.plugin.npm.service.GroupMetadataService;

/**
 * @author Georgy Bolyuba (georgy@bolyuba.com)
 */
public interface NpmGroupRepository
    extends NpmRepository, GroupRepository
{
  @Override
  GroupMetadataService getMetadataService();
}

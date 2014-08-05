package com.bolyuba.nexus.plugin.npm.metadata;

/**
 * Metadata service for group repositories, it aggregates the content of it's members, while deploy is redirected
 * to proper place.
 */
public interface GroupMetadataService
    extends Producer, Consumer
{
}

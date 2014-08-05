package com.bolyuba.nexus.plugin.npm.metadata;

/**
 * Metadata service for hosted repositories, it serves up what has been consumed by it (deployed to it).
 */
public interface HostedMetadataService
    extends Generator, Producer, Consumer
{
}

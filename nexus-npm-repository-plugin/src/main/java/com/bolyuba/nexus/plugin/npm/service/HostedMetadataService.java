package com.bolyuba.nexus.plugin.npm.service;

/**
 * Metadata service for hosted repositories, it serves up what has been consumed by it (deployed to it).
 */
public interface HostedMetadataService
    extends Generator, Consumer
{
}

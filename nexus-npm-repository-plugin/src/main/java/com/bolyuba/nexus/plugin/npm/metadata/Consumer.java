package com.bolyuba.nexus.plugin.npm.metadata;

import java.io.IOException;

import org.sonatype.nexus.proxy.item.ContentLocator;

import com.bolyuba.nexus.plugin.npm.pkg.PackageRequest;

/**
 * Metadata consumer that consumes "raw", probably streamed input of a package root.
 */
public interface Consumer
{
  /**
   * Consumes the package request and it's belonging content into underlying store and returns the consumed package
   * root (merged in applicable, in case update happened).
   */
  PackageRoot consumePackageRoot(PackageRequest request, ContentLocator contentLocator) throws IOException;
}


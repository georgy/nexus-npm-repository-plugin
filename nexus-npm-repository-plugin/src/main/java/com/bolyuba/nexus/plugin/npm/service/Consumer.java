package com.bolyuba.nexus.plugin.npm.service;

import java.io.IOException;

import org.sonatype.nexus.proxy.item.ContentLocator;

/**
 * Metadata consumer that consumes "raw", probably streamed input of a package root.
 */
public interface Consumer
{
  /**
   * Parses the package request and it's belonging content into {@link PackageRoot} instance.
   */
  PackageRoot parsePackageRoot(PackageRequest request, ContentLocator contentLocator) throws IOException;

  /**
   * Consumes the package root into underlying store and returns the consumed package
   * root (merged if applicable, in case update happened).
   */
  PackageRoot consumePackageRoot(PackageRoot packageRoot) throws IOException;
}


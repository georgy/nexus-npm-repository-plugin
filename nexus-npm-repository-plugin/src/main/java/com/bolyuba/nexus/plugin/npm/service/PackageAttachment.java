package com.bolyuba.nexus.plugin.npm.service;

import java.io.File;

import org.sonatype.nexus.proxy.item.FileContentLocator;

import com.bolyuba.nexus.plugin.npm.service.tarball.Tarball;

import static com.google.common.base.Preconditions.checkNotNull;

/**
 * Package attachment.
 *
 * TODO: see {@link Tarball} and possibly drop this class in favor of that one.
 */
public class PackageAttachment
    extends FileContentLocator
{
  private final String name;

  public PackageAttachment(final String name, final File file, final String contentType) {
    super(file, contentType);
    this.name = checkNotNull(name);
  }

  public String getName() {
    return name;
  }
}

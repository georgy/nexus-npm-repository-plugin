package com.bolyuba.nexus.plugin.npm.metadata;

import java.io.File;

import org.sonatype.nexus.proxy.item.FileContentLocator;

import static com.google.common.base.Preconditions.checkNotNull;

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

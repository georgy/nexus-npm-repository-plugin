package com.bolyuba.nexus.plugin.npm.service;

import java.io.File;

import org.sonatype.nexus.proxy.item.FileContentLocator;

import static com.google.common.base.Preconditions.checkNotNull;

/**
 * NPM related binary object, usually tarball.
 */
public class NpmBlob
    extends FileContentLocator
{
  private final String name;

  private final String sha1sum;

  public NpmBlob(final File file, final String contentType, final String name, final String sha1sum) {
    super(file, contentType);
    this.name = checkNotNull(name);
    this.sha1sum = checkNotNull(sha1sum);
  }

  public String getName() {
    return name;
  }

  public String getSha1sum() {
    return sha1sum;
  }
}

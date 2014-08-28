package com.bolyuba.nexus.plugin.npm.service.tarball;

import java.io.File;

import org.sonatype.nexus.proxy.item.AbstractStorageItem;
import org.sonatype.nexus.proxy.item.FileContentLocator;

import com.bolyuba.nexus.plugin.npm.NpmRepository;
import com.bolyuba.nexus.plugin.npm.proxy.DefaultNpmProxyRepository;

/**
 * Tarball with it's freshly calculated SHA1 hash backed by a file. If temporary files used, the temporary file
 * deletion should be taken with care, ie. once it enters local storage the file should be deleted. See {@link
 * DefaultNpmProxyRepository#doCacheItem(AbstractStorageItem)}
 * method.
 */
public class Tarball
    extends FileContentLocator
{
  private final String name;

  private final String originUrl;

  private final String sha1sum;

  public Tarball(final File tempFile, final String name, final String originUrl, final String sha1sum) {
    super(tempFile, NpmRepository.TARBALL_MIME_TYPE, false);
    this.name = name;
    this.originUrl = originUrl;
    this.sha1sum = sha1sum;
  }

  public String getName() { return name; }

  public String getOriginUrl() {
    return originUrl;
  }

  public String getSha1sum() { return sha1sum; }
}

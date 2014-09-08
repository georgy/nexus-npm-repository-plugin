package com.bolyuba.nexus.plugin.npm.service.tarball;

import java.io.IOException;

import com.bolyuba.nexus.plugin.npm.proxy.NpmProxyRepository;
import com.bolyuba.nexus.plugin.npm.service.NpmBlob;

/**
 * Transport for getting NPM tarballs, that might be anywhere (the URL pointed by metadata should be used).
 */
public interface TarballSource
{
  /**
   * Unconditionally fetches the tarball for given package version. This call does not perform any conditional
   * checking of remote, as NPM stores the checksum in the metadata (package version), hence, if locally exists
   * the given file, and checksum matches, no need to check on remote for "newer version". On the other hand, this
   * method will make it's best to ensure that returned tarball is correct content (content validation and transport
   * consistency is checked).
   */
  NpmBlob get(NpmProxyRepository npmProxyRepository, TarballRequest tarballRequest) throws IOException;
}

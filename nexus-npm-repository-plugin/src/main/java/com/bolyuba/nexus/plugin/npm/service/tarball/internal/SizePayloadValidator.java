package com.bolyuba.nexus.plugin.npm.service.tarball.internal;

import javax.inject.Named;
import javax.inject.Singleton;

import org.sonatype.sisu.goodies.common.ComponentSupport;

import com.bolyuba.nexus.plugin.npm.service.NpmBlob;
import com.bolyuba.nexus.plugin.npm.service.tarball.TarballRequest;

/**
 * Basic validator that refuses tarballs having zero length.
 */
@Singleton
@Named
public class SizePayloadValidator
    extends ComponentSupport
    implements TarballValidator
{
  @Override
  public Result validate(final TarballRequest request, final NpmBlob tarball) {
    return tarball.getLength() > 0 ? Result.VALID : Result.INVALID;
  }
}

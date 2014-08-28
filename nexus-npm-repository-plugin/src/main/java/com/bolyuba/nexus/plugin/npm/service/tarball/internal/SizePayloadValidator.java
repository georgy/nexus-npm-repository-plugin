package com.bolyuba.nexus.plugin.npm.service.tarball.internal;

import javax.inject.Named;
import javax.inject.Singleton;

import org.sonatype.sisu.goodies.common.ComponentSupport;

import com.bolyuba.nexus.plugin.npm.service.tarball.Tarball;
import com.bolyuba.nexus.plugin.npm.service.tarball.TarballRequest;

/**
 * Basic validator that refuses tarballs having zero or less length.
 */
@Singleton
@Named
public class SizePayloadValidator
    extends ComponentSupport
    implements TarballValidator
{
  @Override
  public Result validate(final TarballRequest request, final Tarball tarball) {
    return tarball.getLength() > 0 ? Result.VALID : Result.INVALID;
  }
}

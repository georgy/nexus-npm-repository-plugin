package com.bolyuba.nexus.plugin.npm.service.tarball.internal;

import javax.inject.Named;
import javax.inject.Singleton;

import org.sonatype.sisu.goodies.common.ComponentSupport;

import com.bolyuba.nexus.plugin.npm.service.NpmBlob;
import com.bolyuba.nexus.plugin.npm.service.tarball.TarballRequest;

/**
 * Validator that based on expected and calculated (during transport) SHA1 hashes decides is content corrupted or not
 * (invalid or valid).
 */
@Singleton
@Named
public class Sha1HashPayloadValidator
    extends ComponentSupport
    implements TarballValidator
{
  @Override
  public Result validate(final TarballRequest request, final NpmBlob tarball) {
    // checksum validation: if present in metadata (usually is) as repo itself has no policy settings
    final String expectedShasum = request.getPackageVersion().getDistShasum();
    if (expectedShasum != null && !expectedShasum.equals(tarball.getSha1sum())) {
      return Result.INVALID;
    }
    else if (expectedShasum == null) {
      return Result.NEUTRAL;
    }
    else {
      return Result.VALID;
    }
  }
}

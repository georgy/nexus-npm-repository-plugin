package com.bolyuba.nexus.plugin.npm.service.tarball.internal;

import com.bolyuba.nexus.plugin.npm.service.tarball.Tarball;
import com.bolyuba.nexus.plugin.npm.service.tarball.TarballRequest;

/**
 * Tarball validator, validates the tarball content and/or it's properties.
 */
public interface TarballValidator
{
  public enum Result
  {
    INVALID, NEUTRAL, VALID
  }

  /**
   * Validates tarball and cleanly returns if all found clean. Otherwise, preferred way to signal invalid content is to
   * throw {@link IllegalArgumentException}. Never returns {@code null}.
   */
  Result validate(TarballRequest request, Tarball tarball);
}

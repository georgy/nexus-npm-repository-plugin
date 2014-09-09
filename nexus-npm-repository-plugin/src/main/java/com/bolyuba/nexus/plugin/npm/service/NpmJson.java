package com.bolyuba.nexus.plugin.npm.service;

import java.util.Map;

import com.google.common.collect.Maps;

import static com.google.common.base.Preconditions.checkNotNull;

/**
 * Base class for "raw" JSON wrapper (represented as map of String-Object).
 */
abstract class NpmJson
{
  private final String repositoryId;

  private final Map<String, Object> raw;

  public NpmJson(final String repositoryId, final Map<String, Object> raw) {
    this.repositoryId = checkNotNull(repositoryId);
    this.raw = Maps.newHashMap();
    setRaw(raw);
  }

  public String getRepositoryId() {
    return repositoryId;
  }

  public Map<String, Object> getRaw() {
    return raw;
  }

  public void setRaw(final Map<String, Object> raw) {
    validate(raw);
    this.raw.clear();
    this.raw.putAll(raw);
  }

  protected abstract void validate(final Map<String, Object> raw);
}

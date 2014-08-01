package com.bolyuba.nexus.plugin.npm.metadata;

import java.util.Map;

import com.google.common.collect.Maps;

import static com.google.common.base.Preconditions.checkNotNull;

/**
 * Created by cstamas on 29/07/14.
 */
public abstract class NpmJson
{
  private final String repositoryId;

  private final Map<String, String> properties;

  private final Map<String, Object> raw;

  public NpmJson(final String repositoryId, final Map<String, Object> raw) {
    this.repositoryId = checkNotNull(repositoryId);
    this.properties = Maps.newHashMap();
    this.raw = Maps.newHashMap();
    setRaw(raw);
  }

  public String getRepositoryId() {
    return repositoryId;
  }

  public Map<String, Object> getRaw() {
    return raw;
  }

  public Map<String, String> getProperties() { return properties; }

  public void setRaw(final Map<String, Object> raw) {
    validate(raw);
    this.raw.clear();
    this.raw.putAll(raw);
  }

  protected abstract void validate(final Map<String, Object> raw);
}

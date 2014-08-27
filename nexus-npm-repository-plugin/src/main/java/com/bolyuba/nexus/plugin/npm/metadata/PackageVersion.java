package com.bolyuba.nexus.plugin.npm.metadata;

import java.util.Map;

import static com.google.common.base.Preconditions.checkArgument;
import static com.google.common.base.Preconditions.checkNotNull;

public class PackageVersion
    extends NpmJson
{
  public PackageVersion(final String repositoryId, final Map<String, Object> raw) {
    super(repositoryId, raw);
  }

  public String getName() {
    return (String) getRaw().get("name");
  }

  public String getDescription() {
    return (String) getRaw().get("description");
  }

  public String getVersion() {
    return (String) getRaw().get("version");
  }

  public String getDistTarball() {
    return (String) ((Map) getRaw().get("dist")).get("tarball");
  }

  public String getDistTarballFilename() {
    String tarballUrl = getDistTarball();
    int idx = tarballUrl.lastIndexOf("/");
    if (idx != -1) {
      return tarballUrl.substring(idx + 1);
    } else {
      //Unknown tarball, construct default
      return getName() + "-" + getVersion() + ".tgz";
    }
  }

  public void setDistTarball(String tarball) {
    checkNotNull(tarball);
    ((Map) getRaw().get("dist")).put("tarball", tarball);
  }

  /**
   * Returns {@code true} if the {@code dist.tarball} property of this version is unknown. This means, that this
   * document originates most probably from a registry root listing, where version documents are not enlisted. To
   * make a version complete, it needs to be fetched from registry directly, or it's enclosing package root should be
   * fetched. Applies to proxied metadata only.
   */
  public boolean isIncomplete() {
    return "unknown".equals(getDistTarball());
  }

  // ==

  @Override
  protected void validate(final Map<String, Object> raw) {
    checkNotNull(raw);
    checkArgument(raw.containsKey("name"), "No mapping for 'name'");
    checkArgument(raw.get("name") instanceof String, "Mapping for 'name' is not a string");
    checkArgument(raw.containsKey("version"), "No mapping for 'version'");
    checkArgument(raw.get("version") instanceof String, "Mapping for 'version' is not a string");
    checkArgument(raw.containsKey("dist"), "No mapping for 'dist'");
    checkArgument(raw.get("dist") instanceof Map, "'dist' is not an object hash");
    checkArgument(((Map) raw.get("dist")).containsKey("tarball"), "No mapping for 'dist.tarball'");
    checkArgument(((Map) raw.get("dist")).get("tarball") instanceof String,
        "Mapping for 'dist.tarball' is not a string");
  }
}

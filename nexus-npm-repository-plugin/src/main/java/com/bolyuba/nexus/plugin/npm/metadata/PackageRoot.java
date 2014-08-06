package com.bolyuba.nexus.plugin.npm.metadata;

import java.util.Map;
import java.util.Map.Entry;

import com.google.common.collect.Maps;

import static com.google.common.base.Preconditions.checkArgument;
import static com.google.common.base.Preconditions.checkNotNull;

public class PackageRoot
    extends NpmJson
{
  private final Map<String, PackageVersion> wrappedVersions;

  private final Map<String, PackageAttachment> attachments;

  public PackageRoot(final String repositoryId, final Map<String, Object> raw) {
    super(repositoryId, raw);
    this.wrappedVersions = Maps.newHashMap();
    this.wrappedVersions.putAll(wrapVersions(raw));
    this.attachments = Maps.newHashMap();
  }

  public String getComponentId() {return getRepositoryId() + ":" + getName(); }

  public String getName() {
    return (String) getRaw().get("name");
  }

  public String getDescription() {
    return (String) getRaw().get("description");
  }

  public Map<String, PackageVersion> getVersions() {
    return wrappedVersions;
  }

  public Map<String, PackageAttachment> getAttachments() { return attachments; }

  /**
   * Reduces the document backing map to a "shrinked" version where only latest version is
   * contained. This method and format is usable only in registry root metadata and nowhere else
   * should be used.
   */
  public void shrinkToLatestVersionOnly() {
    if (isUnpublished()) {
      return;
    }
    final String latest = ((Map<String, String>) getRaw().get("dist-tags")).get("latest");
    ((Map<String, Object>) getRaw().get("versions")).clear();
    ((Map<String, Object>) getRaw().get("versions")).put(latest, "latest");
    wrappedVersions.clear();
  }

  /**
   * Returns {@code true} if the package is "unreleased" from registry, basically is deleted. In this case
   * the registry retains it's metadata but enlists new versions at all.
   */
  public boolean isUnpublished() {
    return wrappedVersions.isEmpty();
  }

  /**
   * Returns {@code true} if any of the versions in this package root are incomplete. Applies to proxied metadata only.
   *
   * @see PackageVersion#isIncomplete()
   */
  public boolean isIncomplete() {
    if (isUnpublished()) {
      return false;
    }
    for (PackageVersion pv : getVersions().values()) {
      if (pv.isIncomplete()) {
        return true;
      }
    }
    return false;
  }

  /**
   * Overlays given package root onto this package root, probably changing the mappings, or merging some maps.
   */
  public void overlay(final PackageRoot packageRoot) {
    checkArgument(getComponentId().equals(packageRoot.getComponentId()), "Cannot overlay different package roots!");
    overlay(getRaw(), packageRoot.getRaw()); // this changes underlying raw map directly
    this.wrappedVersions.clear();
    this.wrappedVersions.putAll(wrapVersions(getRaw()));
    this.attachments.clear(); // TODO: is clear needed?
    this.attachments.putAll(packageRoot.getAttachments());
  }

  private Map<String, Object> overlay(Map<String, Object> me, Map<String, Object> him) {
    for (String key : him.keySet()) {
      if (him.get(key) instanceof Map && me.get(key) instanceof Map) {
        Map myChild = (Map) me.get(key);
        Map hisChild = (Map) him.get(key);
        me.put(key, overlay(myChild, hisChild));
      }
      else if (him.get(key) instanceof String && me.get(key) instanceof Map) {
        continue; // skip, this is usually versions : { "x.x.x" : "latest" } on incomplete documents
      }
      else {
        me.put(key, him.get(key));
      }
    }
    return me;
  }

  // ==

  @Override
  protected void validate(final Map<String, Object> raw) {
    checkNotNull(raw);
    checkArgument(raw.containsKey("name"), "No mapping for 'name'");
    checkArgument(raw.get("name") instanceof String, "Mapping for 'name' is not a string");
    if (raw.containsKey("versions")) { // unpublished pkg roots have no this node
      checkArgument(raw.containsKey("versions"), "No mapping for 'versions'");
      checkArgument(raw.get("versions") instanceof Map, "'versions' is not an object hash");
    }
  }

  protected Map<String, PackageVersion> wrapVersions(final Map<String, Object> raw) {
    final Map<String, PackageVersion> wrappedVersions = Maps.newHashMap();
    final Map<String, Object> versions = (Map<String, Object>) raw.get("versions");
    if (versions != null) {
      for (Entry<String, Object> versionsEntry : versions.entrySet()) {
        if (versionsEntry.getValue() instanceof Map) {
          wrappedVersions.put(versionsEntry.getKey(),
              new PackageVersion(getRepositoryId(), (Map<String, Object>) versionsEntry.getValue()));
        }
        else if ("latest".equals(versionsEntry.getValue())) {
          // create an "incomplete" document
          final Map<String, Object> latestVersion = Maps.newHashMap();
          latestVersion.put("name", raw.get("name"));
          if (raw.containsKey("description")) {
            latestVersion.put("description", raw.get("description"));
          }
          latestVersion.put("version", versionsEntry.getKey());
          final Map<String, String> dist = Maps.newHashMap();
          dist.put("tarball", "unknown");
          latestVersion.put("dist", dist);
          wrappedVersions.put(versionsEntry.getKey(), new PackageVersion(getRepositoryId(), latestVersion));
        }
      }
    }
    return wrappedVersions;
  }
}

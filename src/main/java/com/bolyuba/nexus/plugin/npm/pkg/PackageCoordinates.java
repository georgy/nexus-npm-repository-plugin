package com.bolyuba.nexus.plugin.npm.pkg;

import javax.annotation.Nonnull;

/**
 * Url type as per http://wiki.commonjs.org/wiki/Packages/Registry#URLs
 *
 * @author <a href="mailto:georgy@bolyuba.com">Georgy Bolyuba</a>
 */
public class PackageCoordinates {

    public static enum Type {

        REGISTRY_ROOT,

        PACKAGE_ROOT,

        PACKAGE_VERSION
    }

    private final Type type;

    private String packageName;

    private String packageVersion;

    public PackageCoordinates() {
        this.type = Type.REGISTRY_ROOT;
    }

    public PackageCoordinates(@Nonnull String packageName) {
        this.packageName = packageName;
        this.type = Type.PACKAGE_ROOT;
    }

    public PackageCoordinates(@Nonnull String packageName, @Nonnull String packageVersion) {
        this.packageName = packageName;
        this.packageVersion = packageVersion;
        this.type = Type.PACKAGE_VERSION;
    }

    public String getPackageName() {
        return packageName;
    }

    public String getPackageVersion() {
        return packageVersion;
    }

    public Type getType() {
        return type;
    }

}

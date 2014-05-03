package com.bolyuba.nexus.plugin.npm.pkg;

import org.sonatype.nexus.proxy.item.RepositoryItemUid;

import javax.annotation.Nonnull;

/**
 * Url type as per http://wiki.commonjs.org/wiki/Packages/Registry#URLs
 *
 * @author <a href="mailto:georgy@bolyuba.com">Georgy Bolyuba</a>
 */
class PackageCoordinates {

    private static final String NPM_REGISTRY_SPECIAL = "/-";

    public static enum Type {

        REGISTRY_ROOT,
        PACKAGE_ROOT,
        PACKAGE_VERSION,
        REGISTRY_SPECIAL
    }

    private PackageCoordinates() {}

    private Type type;

    private String packageName;

    private String packageVersion;

    private String path;

    public String getPackageName() {
        return packageName;
    }

    public String getPackageVersion() {
        return packageVersion;
    }

    public String getPath() {
        return path;
    }

    public Type getType() {
        return type;
    }

    public static PackageCoordinates coordinatesFromUrl(@Nonnull String requestPath) throws InvalidPackageRequestException {
        PackageCoordinates coordinates = new PackageCoordinates();
        coordinates.path = requestPath;

        if (RepositoryItemUid.PATH_SEPARATOR.equals(requestPath)) {
            coordinates.type = Type.REGISTRY_ROOT;
            return coordinates;
        }

        if (requestPath.startsWith(NPM_REGISTRY_SPECIAL)) {
            coordinates.type = Type.REGISTRY_SPECIAL;
            return coordinates;
        }

        String correctedPath =
                requestPath.startsWith(RepositoryItemUid.PATH_SEPARATOR) ?
                        requestPath.substring(1, requestPath.length()) :
                        requestPath;
        String[] explodedPath = correctedPath.split(RepositoryItemUid.PATH_SEPARATOR);

        if (explodedPath.length == 2) {
            coordinates.type = Type.PACKAGE_VERSION;
            coordinates.packageName = explodedPath[0];
            coordinates.packageVersion = explodedPath[1];
            return coordinates;
        }
        if (explodedPath.length == 1) {
            coordinates.type = Type.PACKAGE_ROOT;
            coordinates.packageName = explodedPath[0];
            return coordinates;
        }

        throw new InvalidPackageRequestException("Path " + correctedPath + " cannot be turned into PackageCoordinates");
    }
}

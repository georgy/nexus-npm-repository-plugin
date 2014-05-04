package com.bolyuba.nexus.plugin.npm.pkg;

import org.sonatype.nexus.proxy.ResourceStoreRequest;

import javax.annotation.Nonnull;

import static com.bolyuba.nexus.plugin.npm.pkg.PackageCoordinates.Type.*;
import static com.google.common.base.Preconditions.checkNotNull;

/**
 * Commonjs package request. Represents domain of valid requests including registry "special" like {@code /-/all}
 *
 * @author <a href="mailto:georgy@bolyuba.com">Georgy Bolyuba</a>
 */
public class PackageRequest {

    private final ResourceStoreRequest storeRequest;

    private PackageCoordinates coordinates;

    public PackageRequest(@Nonnull ResourceStoreRequest storeRequest) throws InvalidPackageRequestException {

        this.storeRequest = checkNotNull(storeRequest);
        this.coordinates = PackageCoordinates.coordinatesFromUrl(storeRequest.getRequestPath());
    }

    public ResourceStoreRequest getStoreRequest() {
        return storeRequest;
    }

    public boolean isPackageRoot() {
        return PACKAGE_ROOT == coordinates.getType();
    }

    public boolean isPackageVersion() {
        return PACKAGE_VERSION == coordinates.getType();
    }

    public boolean isRegistrySpecial() {
        return REGISTRY_SPECIAL == coordinates.getType();
    }

    public boolean isPackage() {
        return isPackageRoot() || isPackageVersion();
    }

    public String getPath() {
        return coordinates.getPath();
    }

    public String getName() {
        return coordinates.getPackageName();
    }

    public String getVersion() {
        return coordinates.getPackageVersion();
    }

    @Override
    public String toString() {
        return "PackageRequest{" +
                "storeRequest=" + storeRequest +
                ", coordinates=" + coordinates +
                '}';
    }
}

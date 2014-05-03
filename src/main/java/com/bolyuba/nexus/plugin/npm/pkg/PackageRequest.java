package com.bolyuba.nexus.plugin.npm.pkg;

import org.sonatype.nexus.proxy.ResourceStoreRequest;

import javax.annotation.Nonnull;

import static com.bolyuba.nexus.plugin.npm.pkg.PackageCoordinates.Type.*;
import static com.google.common.base.Preconditions.checkNotNull;

/**
 *  Commonjs package request with coordinates
 *
 * @author <a href="mailto:georgy@bolyuba.com">Georgy Bolyuba</a>
 */
public class PackageRequest {

    private final ResourceStoreRequest storeRequest;

    private final PackageCoordinates coordinates;

    public PackageRequest(@Nonnull ResourceStoreRequest storeRequest,
                          @Nonnull PackageCoordinates coordinates) {

        this.storeRequest = checkNotNull(storeRequest);
        this.coordinates = checkNotNull(coordinates);
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

    public boolean isPackage() {
        return isPackageRoot() || isPackageVersion();
    }
}

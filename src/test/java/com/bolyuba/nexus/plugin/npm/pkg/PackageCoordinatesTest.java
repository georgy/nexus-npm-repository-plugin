package com.bolyuba.nexus.plugin.npm.pkg;

import org.mockito.MockitoAnnotations;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

import static org.testng.Assert.assertEquals;
import static org.testng.Assert.assertNull;
import static org.testng.Assert.fail;

/**
 * @author <a href="mailto:georgy@bolyuba.com">Georgy Bolyuba</a>
 */
public class PackageCoordinatesTest {

    @BeforeMethod
    void setup() {
        MockitoAnnotations.initMocks(this);
    }

    @Test
    public void coordinatesFromUrl_RegistryRoot() throws InvalidPackageRequestException {
        PackageCoordinates coordinates = PackageCoordinates.coordinatesFromUrl("/");

        assertEquals(coordinates.getType(), PackageCoordinates.Type.REGISTRY_ROOT, "Expected registry root coordinates type");
        assertNull(coordinates.getPackageName());
        assertNull(coordinates.getPackageVersion());
    }

    @Test
    public void coordinatesFromUrl_PackageRoot() throws InvalidPackageRequestException {
        PackageCoordinates coordinates = PackageCoordinates.coordinatesFromUrl("/gonogo");

        assertEquals(coordinates.getType(), PackageCoordinates.Type.PACKAGE_ROOT, "Expected package root coordinates type");
        assertEquals(coordinates.getPackageName(), "gonogo");
        assertNull(coordinates.getPackageVersion());
    }

    @Test
    public void coordinatesFromUrl_PackageVersion() throws InvalidPackageRequestException {
        PackageCoordinates coordinates = PackageCoordinates.coordinatesFromUrl("/gonogo/1.42.0");

        assertEquals(coordinates.getType(), PackageCoordinates.Type.PACKAGE_VERSION, "Expected package version coordinates type");
        assertEquals(coordinates.getPackageName(), "gonogo");
        assertEquals(coordinates.getPackageVersion(), "1.42.0");

    }

    @Test(expectedExceptions = InvalidPackageRequestException.class,
            expectedExceptionsMessageRegExp = "Path .* cannot be turned into PackageCoordinates")
    public void coordinatesFromUrl_UnrecognizedCrapAtTheEnd_ShouldThrow() throws InvalidPackageRequestException {
        PackageCoordinates.coordinatesFromUrl("/gonogo/1.42.0/gimmefive");

        fail("Expected getCoordinates to throw IllegalArgumentException");
    }
}

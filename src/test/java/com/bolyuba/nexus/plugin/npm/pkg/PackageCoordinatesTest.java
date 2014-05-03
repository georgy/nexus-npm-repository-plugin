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

        fail("Expected coordinatesFromUrl to throw IllegalArgumentException");
    }

    @Test
    public void coordinatesFromUrl_PackageNameDash_ShouldThrow() throws InvalidPackageRequestException {
        PackageCoordinates coordinates = PackageCoordinates.coordinatesFromUrl("/-/all/");

        assertEquals(coordinates.getType(), PackageCoordinates.Type.REGISTRY_SPECIAL, "Expected registry special coordinates type");
        assertNull(coordinates.getPackageVersion());
        assertNull(coordinates.getPackageName());
    }

    @Test(expectedExceptions = InvalidPackageRequestException.class,
            expectedExceptionsMessageRegExp = "Invalid package name: -gonogo")
    public void coordinatesFromUrl_PackageNameStartWithDash_ShouldThrow() throws InvalidPackageRequestException {
        PackageCoordinates.coordinatesFromUrl("/-gonogo/1.42.0/");
        fail("Expected coordinatesFromUrl to throw IllegalArgumentException");
    }

    @Test(expectedExceptions = InvalidPackageRequestException.class,
            expectedExceptionsMessageRegExp = "Invalid package name: .")
    public void coordinatesFromUrl_PackageNameDot_ShouldThrow() throws InvalidPackageRequestException {
        PackageCoordinates.coordinatesFromUrl("/./1.42.0/");
        fail("Expected coordinatesFromUrl to throw IllegalArgumentException");
    }

    @Test(expectedExceptions = InvalidPackageRequestException.class,
            expectedExceptionsMessageRegExp = "Invalid package name: ..")
    public void coordinatesFromUrl_PackageNameTwoDots_ShouldThrow() throws InvalidPackageRequestException {
        PackageCoordinates.coordinatesFromUrl("/../1.42.0/");
        fail("Expected coordinatesFromUrl to throw IllegalArgumentException");
    }

    @Test(expectedExceptions = InvalidPackageRequestException.class,
            expectedExceptionsMessageRegExp = "Invalid package version: -1.42.0")
    public void coordinatesFromUrl_PackageVersionStartWithDash_ShouldThrow() throws InvalidPackageRequestException {
        PackageCoordinates.coordinatesFromUrl("/gonogo/-1.42.0/");
        fail("Expected coordinatesFromUrl to throw IllegalArgumentException");
    }

    @Test(expectedExceptions = InvalidPackageRequestException.class,
            expectedExceptionsMessageRegExp = "Invalid package version: .")
    public void coordinatesFromUrl_PackageVersionDot_ShouldThrow() throws InvalidPackageRequestException {
        PackageCoordinates.coordinatesFromUrl("/gonogo/./");
        fail("Expected coordinatesFromUrl to throw IllegalArgumentException");
    }

    @Test(expectedExceptions = InvalidPackageRequestException.class,
            expectedExceptionsMessageRegExp = "Invalid package version: ..")
    public void coordinatesFromUrl_PackageVersionTwoDots_ShouldThrow() throws InvalidPackageRequestException {
        PackageCoordinates.coordinatesFromUrl("/gonogo/../");
        fail("Expected coordinatesFromUrl to throw IllegalArgumentException");
    }
}

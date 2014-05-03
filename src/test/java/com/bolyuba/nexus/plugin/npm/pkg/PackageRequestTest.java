package com.bolyuba.nexus.plugin.npm.pkg;

import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.sonatype.nexus.proxy.ResourceStoreRequest;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

import static org.testng.Assert.assertEquals;
import static org.testng.Assert.assertFalse;
import static org.testng.Assert.assertSame;
import static org.testng.Assert.assertTrue;

/**
 * @author Georgy Bolyuba (georgy@bolyuba.com)
 */
public class PackageRequestTest {

    @Mock
    ResourceStoreRequest mockRequest;

    @BeforeMethod
    void setup() {
        MockitoAnnotations.initMocks(this);
    }

    @Test
    public void sameRequestReturned() throws InvalidPackageRequestException {
        Mockito.when(mockRequest.getRequestPath()).thenReturn("/");
        PackageRequest packageRequest = new PackageRequest(mockRequest);

        assertSame(packageRequest.getStoreRequest(), mockRequest, "Expected to get same store request object back");
    }

    @Test
    public void correctPathReturned() throws InvalidPackageRequestException {
        Mockito.when(mockRequest.getRequestPath()).thenReturn("/-/all");
        PackageRequest packageRequest = new PackageRequest(mockRequest);

        assertEquals(packageRequest.getPath(), "/-/all");
    }

    @Test
    public void registryRoot() throws InvalidPackageRequestException {
        Mockito.when(mockRequest.getRequestPath()).thenReturn("/");
        PackageRequest packageRequest = new PackageRequest(mockRequest);

        assertFalse(packageRequest.isPackage());
        assertFalse(packageRequest.isPackageRoot());
        assertFalse(packageRequest.isPackageVersion());
        assertFalse(packageRequest.isRegistrySpecial());
    }

    @Test
    public void packageRoot() throws InvalidPackageRequestException {
        Mockito.when(mockRequest.getRequestPath()).thenReturn("/golem");
        PackageRequest packageRequest = new PackageRequest(mockRequest);

        assertTrue(packageRequest.isPackage());
        assertTrue(packageRequest.isPackageRoot());
        assertFalse(packageRequest.isPackageVersion());
        assertFalse(packageRequest.isRegistrySpecial());
    }

    @Test
    public void packageVersion() throws InvalidPackageRequestException {
        Mockito.when(mockRequest.getRequestPath()).thenReturn("/golem/139.16");
        PackageRequest packageRequest = new PackageRequest(mockRequest);

        assertTrue(packageRequest.isPackage());
        assertFalse(packageRequest.isPackageRoot());
        assertTrue(packageRequest.isPackageVersion());
        assertFalse(packageRequest.isRegistrySpecial());
    }

    @Test
    public void registrySpecial() throws InvalidPackageRequestException {
        Mockito.when(mockRequest.getRequestPath()).thenReturn("/-/all");
        PackageRequest packageRequest = new PackageRequest(mockRequest);

        assertFalse(packageRequest.isPackage());
        assertFalse(packageRequest.isPackageRoot());
        assertFalse(packageRequest.isPackageVersion());
        assertTrue(packageRequest.isRegistrySpecial());
    }
}

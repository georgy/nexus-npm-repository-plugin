package com.bolyuba.nexus.plugin.npm;

import com.google.inject.Provider;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.sonatype.nexus.proxy.RequestContext;
import org.sonatype.nexus.proxy.ResourceStoreRequest;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

import javax.servlet.http.HttpServletRequest;

import static org.mockito.Mockito.when;
import static org.testng.Assert.*;

/**
 * @author <a href="mailto:georgy@bolyuba.com">Georgy Bolyuba</a>
 */
public class NpmUtility_getCoordinates {

    NpmUtility sut;

    RequestContext mockContext;

    @Mock
    Provider<HttpServletRequest> mockRequestProvider;

    @Mock
    HttpServletRequest mockHttpServletRequest;

    @Mock
    ResourceStoreRequest mockRequest;

    @BeforeMethod
    void setup() {
        MockitoAnnotations.initMocks(this);
        mockContext = new RequestContext();

        when(mockRequestProvider.get()).thenReturn(mockHttpServletRequest);
        sut = new NpmUtility(mockRequestProvider);
    }

    @Test(expectedExceptions = IllegalArgumentException.class,
            expectedExceptionsMessageRegExp = "Request path is null, impossible to determine coordinates")
    public void getCoordinates_NullPath_ShouldThrow() {
        when(mockRequest.getRequestPath()).thenReturn(null);

        sut.getCoordinates(mockRequest);

        fail("Expected getCoordinates to throw IllegalArgumentException");
    }

    @Test
    public void getCoordinates_RegistryRoot() {
        when(mockRequest.getRequestPath()).thenReturn("/");

        PackageCoordinates coordinates = sut.getCoordinates(mockRequest);

        assertEquals(coordinates.getType(), PackageCoordinates.Type.REGISTRY_ROOT, "Expected registry root coordinates type");
        assertNull(coordinates.getPackageName());
        assertNull(coordinates.getPackageVersion());
    }

    @Test
    public void getCoordinates_PackageRoot() {
        when(mockRequest.getRequestPath()).thenReturn("/gonogo");

        PackageCoordinates coordinates = sut.getCoordinates(mockRequest);

        assertEquals(coordinates.getType(), PackageCoordinates.Type.PACKAGE_ROOT, "Expected package root coordinates type");
        assertEquals(coordinates.getPackageName(), "gonogo");
        assertNull(coordinates.getPackageVersion());
    }

    @Test
    public void getCoordinates_PackageVersion() {
        when(mockRequest.getRequestPath()).thenReturn("/gonogo/1.42.0");

        PackageCoordinates coordinates = sut.getCoordinates(mockRequest);

        assertEquals(coordinates.getType(), PackageCoordinates.Type.PACKAGE_VERSION, "Expected package version coordinates type");
        assertEquals(coordinates.getPackageName(), "gonogo");
        assertEquals(coordinates.getPackageVersion(), "1.42.0");

    }

    @Test(expectedExceptions = IllegalArgumentException.class,
            expectedExceptionsMessageRegExp = "Path .* cannot be turned into PackageCoordinates")
    public void getCoordinates_UnrecognizedCrapAtTheEnd_ShouldThrow() {
        when(mockRequest.getRequestPath()).thenReturn("/gonogo/1.42.0/gimmefive");

        sut.getCoordinates(mockRequest);

        fail("Expected getCoordinates to throw IllegalArgumentException");
    }
}

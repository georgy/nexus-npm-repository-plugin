package com.bolyuba.nexus.plugin.npm;

import com.bolyuba.nexus.plugin.npm.pkg.PackageRequest;
import com.google.inject.Provider;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.sonatype.nexus.proxy.RequestContext;
import org.sonatype.nexus.proxy.ResourceStoreRequest;
import org.testng.Assert;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

import javax.servlet.http.HttpServletRequest;

import static org.mockito.Mockito.when;
import static org.testng.Assert.*;

/**
 * @author <a href="mailto:georgy@bolyuba.com">Georgy Bolyuba</a>
 */
public class NpmUtility_getContentStorageRequest {

    NpmUtility sut;

    @Mock
    Provider<HttpServletRequest> mockRequestProvider;

    @Mock
    HttpServletRequest mockHttpServletRequest;

    @Mock
    PackageRequest mockRequest;

    @Mock
    ResourceStoreRequest mockOriginalRequest;

    @BeforeMethod
    void setup() {
        MockitoAnnotations.initMocks(this);

        when(mockRequestProvider.get()).thenReturn(mockHttpServletRequest);
        sut = new NpmUtility(mockRequestProvider);
    }

    @Test
    public void getCoordinates_PackageRoot() {
        when(mockRequest.getStoreRequest()).thenReturn(mockOriginalRequest);
        when(mockOriginalRequest.getRequestPath()).thenReturn("/foo");

        ResourceStoreRequest contentStorageRequest = sut.getContentStorageRequest(mockRequest);

        assertEquals(contentStorageRequest.getRequestPath(), "/foo/content.json");
        assertNotSame(contentStorageRequest, mockOriginalRequest);
    }

    @Test
    public void getCoordinates_PackageVersion() {
        when(mockRequest.getStoreRequest()).thenReturn(mockOriginalRequest);
        when(mockOriginalRequest.getRequestPath()).thenReturn("/foo/1.2.3");

        ResourceStoreRequest contentStorageRequest = sut.getContentStorageRequest(mockRequest);

        assertEquals(contentStorageRequest.getRequestPath(), "/foo/1.2.3/content.json");
        assertNotSame(contentStorageRequest, mockOriginalRequest);
    }
}

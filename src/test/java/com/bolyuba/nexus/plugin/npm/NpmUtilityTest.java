package com.bolyuba.nexus.plugin.npm;

import com.google.inject.Provider;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.sonatype.nexus.proxy.RequestContext;
import org.sonatype.nexus.proxy.ResourceStoreRequest;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

import javax.servlet.http.HttpServletRequest;

import static org.mockito.Matchers.anyString;
import static org.mockito.Matchers.same;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.testng.Assert.assertEquals;
import static org.testng.Assert.assertFalse;
import static org.testng.Assert.assertNotSame;
import static org.testng.Assert.assertSame;
import static org.testng.Assert.assertTrue;
import static org.testng.Assert.fail;

/**
 * @author Georgy Bolyuba (georgy@bolyuba.com)
 */
public class NpmUtilityTest {

    @Mock
    ResourceStoreRequest mockRequest;

    @Mock
    RequestContext mockContext;

    @Mock
    Provider<HttpServletRequest> mockRequestProvider;

    @Mock
    HttpServletRequest mockHttpServletRequest;

    NpmUtility sut;

    @BeforeMethod
    void setup() {
        MockitoAnnotations.initMocks(this);

        sut = new NpmUtility(mockRequestProvider);
    }

    @Test
    public void test_isNmpRequest_authOnlyRequest() {
        when(mockRequest.getRequestContext()).thenReturn(mockContext);
        when(mockContext.containsKey(same(RequestContext.CTX_AUTH_CHECK_ONLY))).thenReturn(true);
        when(mockRequestProvider.get()).thenReturn(mockHttpServletRequest);

        assertFalse(sut.isNmpRequest(mockRequest));
    }

    @Test
    public void test_isNmpRequest_json() {
        when(mockRequest.getRequestContext()).thenReturn(mockContext);
        when(mockContext.containsKey(same(RequestContext.CTX_AUTH_CHECK_ONLY))).thenReturn(false);
        when(mockRequestProvider.get()).thenReturn(mockHttpServletRequest);
        when(mockHttpServletRequest.getHeader("accept")).thenReturn("application/json");

        assertTrue(sut.isNmpRequest(mockRequest));
    }

    @Test
    public void test_isNmpRequest_all() {
        when(mockRequest.getRequestContext()).thenReturn(mockContext);
        when(mockContext.containsKey(same(RequestContext.CTX_AUTH_CHECK_ONLY))).thenReturn(false);
        when(mockRequestProvider.get()).thenReturn(mockHttpServletRequest);
        when(mockHttpServletRequest.getHeader("accept")).thenReturn("*/*");

        assertFalse(sut.isNmpRequest(mockRequest));
    }

    @Test(expectedExceptions = IllegalStateException.class,
            expectedExceptionsMessageRegExp = "Container did not provide an instance of HttpServletRequest")
    public void test_isNmpRequest_NoServletRequest_ShouldThrow() {
        when(mockRequest.getRequestContext()).thenReturn(mockContext);
        when(mockContext.containsKey(same(RequestContext.CTX_AUTH_CHECK_ONLY))).thenReturn(false);
        when(mockRequestProvider.get()).thenReturn(null);

        sut.isNmpRequest(mockRequest);
        fail("Expected exception to be thrown");
    }

    @Test
    public void test_wrapRequest_ExpectSameObject() {
        when(mockRequest.getRequestPath()).thenReturn("/package");

        final ResourceStoreRequest request = sut.wrapRequest(mockRequest);

        assertSame(request, mockRequest, "wrapRequest method must return same request object");
    }

    @Test
    public void test_wrapRequest_PackageRoot() {
        when(mockRequest.getRequestPath()).thenReturn("/package");

        sut.wrapRequest(mockRequest);

        verify(mockRequest, times(1)).setRequestPath("/package/-content.json");
    }

    @Test
    public void test_wrapRequest_PackageVersion() {
        when(mockRequest.getRequestPath()).thenReturn("/package/version");

        sut.wrapRequest(mockRequest);

        verify(mockRequest, times(1)).setRequestPath("/package/version/-content.json");
    }

    @Test
    public void test_replaceRequest_ExpectSameObject() {
        when(mockRequest.getRequestPath()).thenReturn("/package");

        final ResourceStoreRequest request = sut.replaceRequest(mockRequest);

        assertNotSame(request, mockRequest, "wrapRequest method must return same request object");
    }

    @Test
    public void test_replaceRequest_PackageRoot() {
        when(mockRequest.getRequestPath()).thenReturn("/package");

        final ResourceStoreRequest request = sut.replaceRequest(mockRequest);

        verify(mockRequest, never()).setRequestPath(anyString());
        assertEquals(request.getRequestPath(), "/package/-content.json");
    }

    @Test
    public void test_replaceRequest_PackageVersion() {
        when(mockRequest.getRequestPath()).thenReturn("/package/version");

        final ResourceStoreRequest request = sut.replaceRequest(mockRequest);

        verify(mockRequest, never()).setRequestPath(anyString());
        assertEquals(request.getRequestPath(), "/package/version/-content.json");
    }
}

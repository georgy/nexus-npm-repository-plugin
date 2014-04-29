package com.bolyuba.nexus.plugin.npm;

import com.bolyuba.nexus.plugin.npm.hosted.NpmHostedRepository;
import com.google.inject.Provider;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.sonatype.nexus.proxy.RequestContext;
import org.sonatype.nexus.proxy.ResourceStoreRequest;
import org.sonatype.nexus.proxy.item.DefaultStorageFileItem;
import org.sonatype.nexus.proxy.item.RepositoryItemUid;
import org.sonatype.nexus.proxy.storage.UnsupportedStorageOperationException;
import org.sonatype.nexus.proxy.storage.local.LocalRepositoryStorage;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

import javax.servlet.http.HttpServletRequest;
import java.io.ByteArrayInputStream;
import java.io.IOException;

import static org.mockito.Mockito.when;
import static org.testng.Assert.assertEquals;
import static org.testng.Assert.assertFalse;
import static org.testng.Assert.assertTrue;

/**
 * @author Georgy Bolyuba (georgy@bolyuba.com)
 */
public class NpmUtilityTest {

    @Mock
    ResourceStoreRequest mockRequest;

    @Mock
    NpmHostedRepository mockRepository;

    @Mock
    DefaultStorageFileItem mockStorageFileItem;

    @Mock
    LocalRepositoryStorage mockLocalStorage;

    @Mock
    Provider<HttpServletRequest> mockRequestProvider;

    @Mock
    HttpServletRequest mockHttpServletRequest;

    RequestContext mockContext;


    NpmUtility sut;

    @BeforeMethod
    void setup () {
        MockitoAnnotations.initMocks(this);
        mockContext = new RequestContext();

        when(mockRequestProvider.get()).thenReturn(mockHttpServletRequest);
        sut = new NpmUtility(mockRequestProvider);
    }

    @Test
    public void test_isNmpRequest_json() {
        when(mockHttpServletRequest.getHeader("accept")).thenReturn("application/json");
        assertTrue(sut.isNmpRequest(mockRequest));
    }

    @Test
    public void test_isNmpRequest_all() {
        when(mockHttpServletRequest.getHeader("accept")).thenReturn("*/*");
        assertFalse(sut.isNmpRequest(mockRequest));
    }



    @Test
    public void test_addNpmMeta_ROOT() {
        when(mockRequest.getRequestPath()).thenReturn(RepositoryItemUid.PATH_ROOT);
        when(mockRequest.getRequestContext()).thenReturn(mockContext);

        sut.addNpmMeta(mockRequest);

        assertTrue(mockContext.isEmpty(), "Expected empty context");
    }

    @Test
    public void test_addNpmMeta_packageRoot() {
        when(mockRequest.getRequestPath()).thenReturn(RepositoryItemUid.PATH_ROOT + "socket.io");
        when(mockRequest.getRequestContext()).thenReturn(mockContext);

        sut.addNpmMeta(mockRequest);

        assertTrue(mockContext.containsKey(NpmUtility.NPM_PACKAGE), "Expected package item");
        assertEquals(mockContext.get(NpmUtility.NPM_PACKAGE), "socket.io");
        assertEquals(mockContext.size(), 1, "Expected exactly 1 item");
    }

    @Test
    public void test_addNpmMeta_packageVersion() {
        when(mockRequest.getRequestPath()).thenReturn(RepositoryItemUid.PATH_ROOT + "gonogo" + RepositoryItemUid.PATH_SEPARATOR + "0.0.42");
        when(mockRequest.getRequestContext()).thenReturn(mockContext);

        sut.addNpmMeta(mockRequest);

        assertTrue(mockContext.containsKey(NpmUtility.NPM_PACKAGE), "Expected package item");
        assertEquals(mockContext.get(NpmUtility.NPM_PACKAGE), "gonogo");

        assertTrue(mockContext.containsKey(NpmUtility.NPM_VERSION), "Expected version item");
        assertEquals(mockContext.get(NpmUtility.NPM_VERSION), "0.0.42");

        assertEquals(mockContext.size(), 2, "Expected exactly 2 items");
    }

    @Test
    public void test_processStoreRequest() throws IOException, UnsupportedStorageOperationException {
        when(mockStorageFileItem.getPath()).thenReturn(NpmUtility.HIDDEN_CACHE_PREFIX + "/foo-app");

        ByteArrayInputStream is = new ByteArrayInputStream("{\"name\": \"foo-app\", \"versions\": { \"0.0.1\": { \"name\": \"z-my-test-app\", \"description\": \"App I use to test npm registry plugin for Nexus\", \"author\": { \"name\": \"Georgy Bolyuba\", \"email\": \"georgy@bolyuba.com\"}}}}".getBytes());
        when(mockStorageFileItem.getInputStream()).thenReturn(is);

        when(mockRepository.getLocalStorage()).thenReturn(mockLocalStorage);
        when(mockRepository.getId()).thenReturn("mock-repo");

        sut.processStoreRequest(mockStorageFileItem, mockRepository);
    }
}

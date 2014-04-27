package com.bolyuba.nexus.plugin.npm;

import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.sonatype.nexus.proxy.RequestContext;
import org.sonatype.nexus.proxy.ResourceStoreRequest;
import org.sonatype.nexus.proxy.item.RepositoryItemUid;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

import static org.mockito.Mockito.when;
import static org.testng.Assert.assertEquals;
import static org.testng.Assert.assertTrue;

/**
 * @author Georgy Bolyuba (georgy@bolyuba.com)
 */
public class NpmUtilityTest {

    @Mock
    ResourceStoreRequest mockRequest;

    RequestContext mockContext;


    NpmUtility sut;

    @BeforeMethod
    void setup () {
        MockitoAnnotations.initMocks(this);
        mockContext = new RequestContext();
        sut = new NpmUtility();
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
}

package com.bolyuba.nexus.plugin.npm;

import com.bolyuba.nexus.plugin.npm.hosted.NpmHostedRepository;
import com.google.inject.Provider;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.sonatype.nexus.proxy.AccessDeniedException;
import org.sonatype.nexus.proxy.IllegalOperationException;
import org.sonatype.nexus.proxy.ItemNotFoundException;
import org.sonatype.nexus.proxy.RequestContext;
import org.sonatype.nexus.proxy.ResourceStoreRequest;
import org.sonatype.nexus.proxy.item.DefaultStorageFileItem;
import org.sonatype.nexus.proxy.storage.UnsupportedStorageOperationException;
import org.sonatype.nexus.proxy.storage.local.LocalRepositoryStorage;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

import javax.servlet.http.HttpServletRequest;
import java.io.ByteArrayInputStream;
import java.io.IOException;

import static org.mockito.Mockito.when;

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
    public void test_processStoreRequest() throws IOException, UnsupportedStorageOperationException, ItemNotFoundException, AccessDeniedException, IllegalOperationException {
        when(mockStorageFileItem.getPath()).thenReturn(NpmUtility.HIDDEN_CACHE_PREFIX + "/foo-app");

        ByteArrayInputStream is = new ByteArrayInputStream("{\"name\": \"foo-app\", \"versions\": { \"0.0.1\": { \"name\": \"z-my-test-app\", \"description\": \"App I use to test npm registry plugin for Nexus\", \"author\": { \"name\": \"Georgy Bolyuba\", \"email\": \"georgy@bolyuba.com\"}}}}".getBytes());
        when(mockStorageFileItem.getInputStream()).thenReturn(is);

        when(mockRepository.getLocalStorage()).thenReturn(mockLocalStorage);
        when(mockRepository.getId()).thenReturn("mock-repo");

        sut.processStoreRequest(mockStorageFileItem, mockRepository);
    }
}

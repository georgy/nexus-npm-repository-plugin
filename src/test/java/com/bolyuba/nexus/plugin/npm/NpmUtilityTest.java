package com.bolyuba.nexus.plugin.npm;

import com.bolyuba.nexus.plugin.npm.hosted.NpmHostedRepository;
import com.google.inject.Provider;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.sonatype.nexus.proxy.RequestContext;
import org.sonatype.nexus.proxy.ResourceStoreRequest;
import org.sonatype.nexus.proxy.item.DefaultStorageFileItem;
import org.sonatype.nexus.proxy.storage.local.LocalRepositoryStorage;
import org.testng.annotations.BeforeMethod;

import javax.servlet.http.HttpServletRequest;

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
}

package com.bolyuba.nexus.plugin.npm.proxy;

import com.bolyuba.nexus.plugin.npm.NpmUtility;
import com.bolyuba.nexus.plugin.npm.content.NpmMimeRulesSource;
import com.google.inject.Provider;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.sonatype.nexus.proxy.LocalStorageException;
import org.sonatype.nexus.proxy.ResourceStoreRequest;
import org.sonatype.nexus.proxy.item.AbstractStorageItem;
import org.sonatype.nexus.proxy.item.DefaultStorageFileItem;
import org.sonatype.nexus.proxy.registry.ContentClass;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

import javax.servlet.http.HttpServletRequest;

import static org.mockito.Matchers.any;
import static org.mockito.Matchers.same;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.testng.Assert.assertSame;

/**
 * @author Georgy Bolyuba (georgy@bolyuba.com)
 */
public class DefaultNpmProxyRepository_doCacheItem {

    @Mock
    ContentClass mockContentClass;

    @Mock
    NpmMimeRulesSource mockMimeRulesSource;

    @Mock
    NpmUtility mockNpmUtility;

    @Mock
    DefaultStorageFileItem mockStorageFileItem;

    @Mock
    ResourceStoreRequest mockStoreRequest;

    @Mock
    DefaultStorageFileItem mockWrappedDefaultStorageFileItem;

    @Mock
    Provider<HttpServletRequest> mockRequestProvider;

    DefaultNpmProxyRepository sut;

    @BeforeMethod
    void setup() throws LocalStorageException {
        MockitoAnnotations.initMocks(this);
    }

    @Test
    public void shouldCachePackageVersion() throws LocalStorageException {

        sut = spy(
                new DefaultNpmProxyRepository(
                        mockContentClass,
                        new NpmProxyRepositoryConfigurator(),
                        mockRequestProvider)
        );

        // mock real call to doCacheItem
        doReturn(mockStorageFileItem).when(sut).delegateDoCacheItem(mockWrappedDefaultStorageFileItem);
        // mock wrapping for now
        doReturn(mockWrappedDefaultStorageFileItem).when(sut).wrapItem(same(mockStorageFileItem));

        when(mockStorageFileItem.getResourceStoreRequest()).thenReturn(mockStoreRequest);
        when(mockStoreRequest.getRequestPath()).thenReturn("/gonogo/1.0.42");

        AbstractStorageItem abstractStorageItem = sut.doCacheItem(mockStorageFileItem);

        assertSame(abstractStorageItem, mockStorageFileItem);
        verify(sut, times(1)).delegateDoCacheItem(mockWrappedDefaultStorageFileItem);
    }

    @Test
    public void shouldCachePackageRoot() throws LocalStorageException {

        sut = spy(
                new DefaultNpmProxyRepository(
                        mockContentClass,
                        new NpmProxyRepositoryConfigurator(),
                        mockRequestProvider)
        );

        // mock real call to doCacheItem
        doReturn(mockStorageFileItem).when(sut).delegateDoCacheItem(mockWrappedDefaultStorageFileItem);
        // mock wrapping for now
        doReturn(mockWrappedDefaultStorageFileItem).when(sut).wrapItem(same(mockStorageFileItem));

        when(mockStorageFileItem.getResourceStoreRequest()).thenReturn(mockStoreRequest);
        when(mockStoreRequest.getRequestPath()).thenReturn("/gonogo");

        AbstractStorageItem abstractStorageItem = sut.doCacheItem(mockStorageFileItem);

        assertSame(abstractStorageItem, mockStorageFileItem);
        verify(sut, times(1)).delegateDoCacheItem(mockWrappedDefaultStorageFileItem);
    }

    @Test
    public void shouldNotCacheRegistryRoot() throws LocalStorageException {

        sut = spy(
                new DefaultNpmProxyRepository(
                        mockContentClass,
                        new NpmProxyRepositoryConfigurator(),
                        mockRequestProvider)
        );

        when(mockStorageFileItem.getResourceStoreRequest()).thenReturn(mockStoreRequest);
        when(mockStoreRequest.getRequestPath()).thenReturn("/");

        AbstractStorageItem abstractStorageItem = sut.doCacheItem(mockStorageFileItem);

        assertSame(abstractStorageItem, mockStorageFileItem);
        verify(sut, never()).delegateDoCacheItem(any(AbstractStorageItem.class));
    }

    @Test
    public void shouldCacheTarball() throws LocalStorageException {

        sut = spy(
                new DefaultNpmProxyRepository(
                        mockContentClass,
                        new NpmProxyRepositoryConfigurator(),
                        mockRequestProvider)
        );

        // caching original item mockStorageFileItem
        doReturn(mockStorageFileItem).when(sut).delegateDoCacheItem(mockStorageFileItem);

        when(mockStorageFileItem.getResourceStoreRequest()).thenReturn(mockStoreRequest);
        when(mockStoreRequest.getRequestPath()).thenReturn("/gonogo/-/gonogo-1.42.0.tgz");

        AbstractStorageItem abstractStorageItem = sut.doCacheItem(mockStorageFileItem);

        assertSame(abstractStorageItem, mockStorageFileItem);
        verify(sut, times(1)).delegateDoCacheItem(any(AbstractStorageItem.class));
    }
}

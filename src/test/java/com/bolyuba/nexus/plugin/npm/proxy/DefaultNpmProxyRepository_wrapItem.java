package com.bolyuba.nexus.plugin.npm.proxy;

import com.bolyuba.nexus.plugin.npm.NpmUtility;
import com.bolyuba.nexus.plugin.npm.content.NpmMimeRulesSource;
import com.bolyuba.nexus.plugin.npm.content.TarballUrlFilteringContentLocator;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.sonatype.nexus.proxy.LocalStorageException;
import org.sonatype.nexus.proxy.ResourceStoreRequest;
import org.sonatype.nexus.proxy.item.AbstractStorageItem;
import org.sonatype.nexus.proxy.item.ContentLocator;
import org.sonatype.nexus.proxy.item.DefaultStorageFileItem;
import org.sonatype.nexus.proxy.registry.ContentClass;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

import static org.mockito.Matchers.notNull;
import static org.mockito.Matchers.same;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.testng.Assert.assertNotSame;

/**
 * @author Georgy Bolyuba (georgy@bolyuba.com)
 */
public class DefaultNpmProxyRepository_wrapItem {

    @Mock ContentClass mockContentClass;

    @Mock NpmMimeRulesSource mockMimeRulesSource;

    @Mock NpmUtility mockNpmUtility;

    @Mock DefaultStorageFileItem mockStorageFileItem;

    @Mock ResourceStoreRequest mockStoreRequest;

    @Mock DefaultStorageFileItem mockWrappedDefaultStorageFileItem;

    @Mock ContentLocator mockContentLocator;

    @Mock NpmUtility npmUtility;

    DefaultNpmProxyRepository sut;

    @BeforeMethod
    void setup() throws LocalStorageException {
        MockitoAnnotations.initMocks(this);
    }

    @Test
    public void wrapPackageVersion() throws LocalStorageException {

        sut = spy(
                new DefaultNpmProxyRepository(
                        mockContentClass,
                        new NpmProxyRepositoryConfigurator(),
                        npmUtility)
        );

        prepareStorageItem();

        when(mockStoreRequest.getRequestUrl()).thenReturn("http://localhost:8081/nexus/content/npm/registry.npmjs.org/gonogo/1.42.0");
        when(mockStoreRequest.getRequestPath()).thenReturn("/gonogo/1.42.0");

        doReturn("https://registry.npmjs.org/gonogo/1.42.0").when(sut).getRemoteUrl();
        mockOutCreationOfWrappedItem();

        AbstractStorageItem abstractStorageItem = sut.wrapItem(mockStorageFileItem);

        assertNotSame(abstractStorageItem, mockStorageFileItem);
        verify(npmUtility, times(1)).wrapRequest(mockStoreRequest);
    }

    @Test
    public void wrapPackageRoot() throws LocalStorageException {

        sut = spy(
                new DefaultNpmProxyRepository(
                        mockContentClass,
                        new NpmProxyRepositoryConfigurator(),
                        npmUtility)
        );

        prepareStorageItem();

        when(mockStoreRequest.getRequestUrl()).thenReturn("http://localhost:8081/nexus/content/npm/registry.npmjs.org/gonogo");
        when(mockStoreRequest.getRequestPath()).thenReturn("/gonogo");

        doReturn("https://registry.npmjs.org/gonogo").when(sut).getRemoteUrl();
        mockOutCreationOfWrappedItem();

        AbstractStorageItem abstractStorageItem = sut.wrapItem(mockStorageFileItem);

        assertNotSame(abstractStorageItem, mockStorageFileItem);
        verify(npmUtility, times(1)).wrapRequest(mockStoreRequest);
    }

    private void mockOutCreationOfWrappedItem() {
        doReturn(mockWrappedDefaultStorageFileItem).when(sut).getWrappedStorageFileItem(
                same(mockStorageFileItem),
                notNull(TarballUrlFilteringContentLocator.class),
                same(mockStoreRequest));
    }

    private void prepareStorageItem() {
        when(mockStorageFileItem.getResourceStoreRequest()).thenReturn(mockStoreRequest);
        when(mockStorageFileItem.getContentLocator()).thenReturn(mockContentLocator);
        when(mockStorageFileItem.isReadable()).thenReturn(true);
        when(mockStorageFileItem.isWritable()).thenReturn(true);
    }
}

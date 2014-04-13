package com.bolyuba.nexus.plugin.npm.proxy;

import com.bolyuba.nexus.plugin.npm.NpmContentClass;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.sonatype.nexus.proxy.item.RepositoryItemUid;
import org.sonatype.nexus.proxy.repository.Repository;
import org.sonatype.nexus.proxy.repository.RepositoryKind;

import static org.mockito.Mockito.when;

/**
 * @author Georgy Bolyuba (georgy@bolyuba.com)
 */
public class DefaultNpmProxyRepositoryTest {

    @Mock
    RepositoryItemUid mockUid;

    @Mock
    Repository mockRepository;

    @Mock
    RepositoryKind mockKind;

    @Before
    public void setup() {
        MockitoAnnotations.initMocks(this);
    }

    @Test
    public void test_IsNpmMainIndex_OK() {
        DefaultNpmProxyRepository sut = new DefaultNpmProxyRepository(
                new NpmContentClass(),
                new NpmProxyRepositoryConfigurator()
        ) {
            @Override
            boolean isNpmRepo(Repository repository) {
                return true;
            }
        };

        when(mockUid.getRepository()).thenReturn(mockRepository);
        when(mockUid.getPath()).thenReturn("/-/all");

        Assert.assertEquals("Resource must be recognized as npm index", true, sut.isNpmIndex(mockUid));
    }

    @Test
    public void test_isNpmRepo_Ok() {
        DefaultNpmProxyRepository sut = new DefaultNpmProxyRepository(
                new NpmContentClass(),
                new NpmProxyRepositoryConfigurator()
        );

        when(mockRepository.getRepositoryKind()).thenReturn(mockKind);
        when(mockKind.isFacetAvailable(NpmProxyRepository.class)).thenReturn(true);

        Assert.assertEquals("Repository must be recognized as npm repository", true, sut.isNpmRepo(mockRepository));
    }
}

package org.sonatype.nexus.testsuite.npm;

import java.io.File;

import javax.annotation.Nullable;

import org.sonatype.sisu.litmus.testsupport.TestData;

import com.bolyuba.nexus.plugin.npm.client.NpmProxyRepository;
import org.junit.After;
import org.junit.Before;

/**
 * Support for NPM integration tests for trivial cases, when one single NPM mock registry should be started. This test
 * should not be used for cases that would involve multiple registries, like testing NPM groups of multiple NPM
 * proxies, etc. This test uses JUnit {@link @Before} and {@link @After} to start and stop the NPM registry and offers
 * one method to query it's URL.
 */
public abstract class NpmMockRegistryITSupport
    extends NpmITSupport
{
  private MockNpmRegistry mockNpmRegistry;

  public NpmMockRegistryITSupport(final String nexusBundleCoordinates) {
    super(nexusBundleCoordinates);
  }

  /**
   * Starts mock NPM registry if directory named "registry" is found among test data.
   *
   * @see TestData
   */
  @Before
  public void startMockNpmRegistryServer()
      throws Exception
  {
    final File registryRoot = testData().resolveFile("registry");
    if (registryRoot.isDirectory()) {
      mockNpmRegistry = new MockNpmRegistry(registryRoot).start();
    }
  }

  /**
   * Stops mock NPM registry if it was started.
   *
   * @see #startMockNpmRegistryServer()
   */
  @After
  public void stopMockNpmRegistryServer()
      throws Exception
  {
    if (mockNpmRegistry != null) {
      mockNpmRegistry.stop();
    }
  }

  /**
   * If mock NPM registry is started, returns it's root URL as String, {@code null} otherwise.
   *
   * @see #startMockNpmRegistryServer()
   */
  @Nullable
  public String mockRegistryServerUrl() {
    if (mockNpmRegistry == null) {
      return null;
    }
    return mockNpmRegistry.getUrl();
  }

  /**
   * Creates a NPM Proxy repository in NX instance for this ITs mock NPM registry..
   */
  public NpmProxyRepository createNpmProxyRepository(final String id) {
    return createNpmProxyRepository(id, mockRegistryServerUrl());
  }
}
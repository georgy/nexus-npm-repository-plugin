package org.sonatype.nexus.testsuite.npm;

import java.io.File;
import java.util.Collection;

import javax.annotation.Nullable;

import org.sonatype.nexus.bundle.launcher.NexusBundleConfiguration;
import org.sonatype.nexus.client.core.subsystem.content.Content;
import org.sonatype.nexus.client.core.subsystem.repository.Repositories;
import org.sonatype.nexus.testsuite.support.NexusRunningParametrizedITSupport;
import org.sonatype.nexus.testsuite.support.NexusStartAndStopStrategy;
import org.sonatype.sisu.litmus.testsupport.TestData;

import com.bolyuba.nexus.plugin.npm.client.NpmProxyRepository;
import org.junit.After;
import org.junit.Before;
import org.junit.Rule;
import org.junit.rules.ExpectedException;
import org.junit.runners.Parameterized;

import static org.sonatype.nexus.testsuite.support.NexusStartAndStopStrategy.Strategy.EACH_TEST;
import static org.sonatype.nexus.testsuite.support.ParametersLoaders.firstAvailableTestParameters;
import static org.sonatype.nexus.testsuite.support.ParametersLoaders.systemTestParameters;
import static org.sonatype.nexus.testsuite.support.ParametersLoaders.testParameters;
import static org.sonatype.sisu.goodies.common.Varargs.$;

/**
 * Support for NPM integration tests.
 */
@NexusStartAndStopStrategy(EACH_TEST)
public abstract class NpmITSupport
    extends NexusRunningParametrizedITSupport
{
  @Parameterized.Parameters
  public static Collection<Object[]> data() {
    return firstAvailableTestParameters(systemTestParameters(), testParameters(
        $("${it.nexus.bundle.groupId}:${it.nexus.bundle.artifactId}:zip:bundle"))).load();
  }

  @Rule
  public ExpectedException thrown = ExpectedException.none();

  public NpmITSupport(final String nexusBundleCoordinates) {
    super(nexusBundleCoordinates);
  }

  @Override
  protected NexusBundleConfiguration configureNexus(final NexusBundleConfiguration configuration) {
    return super.configureNexus(configuration).setLogLevel("com.bolyuba.nexus.plugin.npm", "DEBUG").addPlugins(
        artifactResolver().resolvePluginFromDependencyManagement("com.bolyuba.nexus.plugin",
            "nexus-npm-repository-plugin"));
  }

  private MockNpmRegistry mockNpmRegistry;

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
   * Creates a NPM Proxy repository in NX instance.
   */
  public NpmProxyRepository createNpmProxyRepository(final String id) {
    return repositories().create(NpmProxyRepository.class, id)
        .asProxyOf(mockRegistryServerUrl()).withName("id").save();
  }

  public Content content() {
    return client().getSubsystem(Content.class);
  }

  public Repositories repositories() {
    return client().getSubsystem(Repositories.class);
  }
}
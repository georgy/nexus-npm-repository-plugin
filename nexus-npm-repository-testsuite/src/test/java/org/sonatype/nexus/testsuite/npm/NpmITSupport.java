package org.sonatype.nexus.testsuite.npm;

import java.util.Collection;

import org.sonatype.nexus.bundle.launcher.NexusBundleConfiguration;
import org.sonatype.nexus.client.core.subsystem.content.Content;
import org.sonatype.nexus.client.core.subsystem.repository.Repositories;
import org.sonatype.nexus.testsuite.support.NexusRunningParametrizedITSupport;
import org.sonatype.nexus.testsuite.support.NexusStartAndStopStrategy;

import com.bolyuba.nexus.plugin.npm.client.NpmProxyRepository;
import org.junit.Rule;
import org.junit.rules.ExpectedException;
import org.junit.runners.Parameterized;

import static com.google.common.base.Preconditions.checkNotNull;
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
    return super.configureNexus(configuration)
        .setLogLevel("com.bolyuba.nexus.plugin.npm", "DEBUG")
        .addPlugins(
            artifactResolver().resolvePluginFromDependencyManagement("com.bolyuba.nexus.plugin",
                "nexus-npm-repository-plugin"));
  }

  /**
   * Creates a NPM Proxy repository in NX instance.
   */
  public NpmProxyRepository createNpmProxyRepository(final String id, final String registryUrl) {
    checkNotNull(id);
    checkNotNull(registryUrl);
    return repositories().create(NpmProxyRepository.class, id)
        .asProxyOf(registryUrl).withName("id").save();
  }

  /**
   * The {@link Content} client.
   */
  public Content content() {
    return client().getSubsystem(Content.class);
  }

  /**
   * The {@link Repositories} client.
   */
  public Repositories repositories() {
    return client().getSubsystem(Repositories.class);
  }
}
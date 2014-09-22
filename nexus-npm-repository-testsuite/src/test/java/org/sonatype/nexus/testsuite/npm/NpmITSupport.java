/*
 * Copyright (c) 2007-2014 Sonatype, Inc. All rights reserved.
 *
 * This program is licensed to you under the Apache License Version 2.0,
 * and you may not use this file except in compliance with the Apache License Version 2.0.
 * You may obtain a copy of the Apache License Version 2.0 at http://www.apache.org/licenses/LICENSE-2.0.
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the Apache License Version 2.0 is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the Apache License Version 2.0 for the specific language governing permissions and limitations there under.
 */
package org.sonatype.nexus.testsuite.npm;

import java.util.Collection;

import org.sonatype.nexus.bundle.launcher.NexusBundleConfiguration;
import org.sonatype.nexus.client.core.subsystem.content.Content;
import org.sonatype.nexus.client.core.subsystem.repository.Repositories;
import org.sonatype.nexus.testsuite.support.NexusRunningParametrizedITSupport;
import org.sonatype.nexus.testsuite.support.NexusStartAndStopStrategy;

import com.bolyuba.nexus.plugin.npm.client.NpmGroupRepository;
import com.bolyuba.nexus.plugin.npm.client.NpmHostedRepository;
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
            artifactResolver().resolvePluginFromDependencyManagement(
                "org.sonatype.nexus.plugins", "nexus-npm-repository-plugin"
            ));
  }

  /**
   * Creates a NPM hosted repository in NX instance.
   */
  public NpmHostedRepository createNpmHostedRepository(final String id) {
    checkNotNull(id);
    return repositories().create(NpmHostedRepository.class, id).withName(id).save();
  }

  /**
   * Creates a NPM Proxy repository in NX instance.
   */
  public NpmProxyRepository createNpmProxyRepository(final String id, final String registryUrl) {
    checkNotNull(id);
    checkNotNull(registryUrl);
    return repositories().create(NpmProxyRepository.class, id)
        .asProxyOf(registryUrl).withName(id).save();
  }

  /**
   * Creates a NPM Group repository in NX instance.
   */
  public NpmGroupRepository createNpmGroupRepository(final String id, String... members) {
    checkNotNull(id);
    return repositories().create(NpmGroupRepository.class, id).withName(id).addMember(members).save();
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
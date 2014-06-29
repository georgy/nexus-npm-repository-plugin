/*
 * Sonatype Nexus (TM) Open Source Version
 * Copyright (c) 2007-2014 Sonatype, Inc.
 * All rights reserved. Includes the third-party code listed at http://links.sonatype.com/products/nexus/oss/attributions.
 *
 * This program and the accompanying materials are made available under the terms of the Eclipse Public License Version 1.0,
 * which accompanies this distribution and is available at http://www.eclipse.org/legal/epl-v10.html.
 *
 * Sonatype Nexus (TM) Professional Version is available from Sonatype, Inc. "Sonatype" and "Sonatype Nexus" are trademarks
 * of Sonatype, Inc. Apache Maven is a trademark of the Apache Software Foundation. M2eclipse is a trademark of the
 * Eclipse Foundation. All other trademarks are the property of their respective owners.
 */
package org.sonatype.nexus.testsuite.npm;

import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Rule;
import org.junit.rules.ExpectedException;
import org.junit.runners.Parameterized;
import org.sonatype.nexus.bundle.launcher.NexusBundleConfiguration;
import org.sonatype.nexus.client.core.subsystem.content.Content;
import org.sonatype.nexus.testsuite.support.NexusRunningParametrizedITSupport;
import org.sonatype.nexus.testsuite.support.NexusStartAndStopStrategy;
import org.sonatype.tests.http.server.fluent.Server;
import org.sonatype.tests.http.server.jetty.behaviour.ErrorBehaviour;

import java.util.Collection;

import static org.sonatype.nexus.testsuite.support.NexusStartAndStopStrategy.Strategy.EACH_TEST;
import static org.sonatype.nexus.testsuite.support.ParametersLoaders.*;
import static org.sonatype.sisu.goodies.common.Varargs.$;

/**
 * Support for NPM integration tests.
 *
 * @since 2.9
 */
@NexusStartAndStopStrategy(EACH_TEST)
public class NpmITSupport
    extends NexusRunningParametrizedITSupport
{
    @Parameterized.Parameters
    public static Collection<Object[]> data() {
        return firstAvailableTestParameters(
            systemTestParameters(),
            testParameters(
                $("${it.nexus.bundle.groupId}:${it.nexus.bundle.artifactId}:zip:bundle")
            )
        ).load();
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
                    "com.bolyuba.nexus.plugin", "nexus-npm-repository-plugin"
                )
            );
    }

    private static Server server;

    @BeforeClass
    public static void startMockRegistryServer()
        throws Exception
    {
        server = Server.withPort(0).serve("/*").withBehaviours(new ErrorBehaviour(404, "not found")).start();
    }

    @AfterClass
    public static void stopMockRegistryServer()
        throws Exception
    {
        if (server != null) {
            server.stop();
        }
    }

    public Content content() {
        return client().getSubsystem(Content.class);
    }

}
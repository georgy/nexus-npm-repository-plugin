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
package org.sonatype.nexus.testsuite.npm.smoke;

import java.io.File;

import org.sonatype.nexus.client.core.subsystem.content.Location;
import org.sonatype.nexus.testsuite.npm.NpmMockRegistryITSupport;

import com.google.common.base.Charsets;
import com.google.common.io.Files;
import org.junit.Test;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsString;
import static org.hamcrest.Matchers.not;

/**
 * Smoke IT for NPM plugin. This IT just starts up NX with NPM plugin, creates a NPM proxy repo, and downloads
 * one single metadata file from it, and validates that URLs are properly rewritten to point back to NX instance. If
 * any
 * of these fails, this IT will fail too.
 */
public class SmokeNpmIT
    extends NpmMockRegistryITSupport
{
  public SmokeNpmIT(String nexusBundleCoordinates) {
    super(nexusBundleCoordinates);
  }

  @Test
  public void smoke() throws Exception {
    // create a NPM Proxy repository that proxies mock NPM registry
    createNpmProxyRepository(testMethodName());

    // download package root of commonjs
    final File localDirectory = util.createTempDir();
    final File commonjsPackageRootFile = new File(localDirectory, "commonjs.json");
    content().download(Location.repositoryLocation(testMethodName(), "commonjs"), commonjsPackageRootFile);
    final String commonjsPackageRoot = Files.toString(commonjsPackageRootFile, Charsets.UTF_8);

    // check are the URLs rewritten and point back to NX
    assertThat(commonjsPackageRoot, containsString(
        nexus().getUrl() + "content/repositories/" + testMethodName() + "/commonjs/-/commonjs-0.0.1.tgz"));

    // check that there are not traces of proxied registry URL
    assertThat(commonjsPackageRoot, not(containsString(mockRegistryServerUrl())));
  }
}
package org.sonatype.nexus.testsuite.npm.smoke;

import java.io.File;

import org.sonatype.nexus.client.core.subsystem.content.Location;
import org.sonatype.nexus.client.core.subsystem.repository.Repositories;
import org.sonatype.nexus.testsuite.npm.NpmITSupport;

import com.bolyuba.nexus.plugin.npm.client.NpmProxyRepository;
import com.google.common.base.Charsets;
import com.google.common.io.Files;
import org.junit.Test;

import static org.hamcrest.MatcherAssert.*;
import static org.hamcrest.Matchers.*;

/**
 * Smoke IT for NPM plugin. This IT just starts up NX with NPM plugin, creates a NPM proxy repo, and downloads
 * one single metadata file from it, and validates that URLs are properly rewritten to point back to NX instance. If any
 * of these fails, this IT will fail too.
 */
public class SmokeNpmIT
    extends NpmITSupport
{
  private static String REPO_ID = "npmproxy";

  public SmokeNpmIT(String nexusBundleCoordinates) {
    super(nexusBundleCoordinates);
  }

  @Test
  public void smoke() throws Exception {
    // create a NPM Proxy repository that proxies mock NPM registry
    client().getSubsystem(Repositories.class).create(NpmProxyRepository.class, REPO_ID)
        .asProxyOf(mockRegistryServerUrl()).withName("NPM Proxy").save();

    // download package root of commonjs
    final File localDirectory = util.createTempDir();
    final File commonjsPackageRootFile = new File(localDirectory, "commonjs.json");
    content().download(Location.repositoryLocation(REPO_ID, "commonjs"), commonjsPackageRootFile);
    final String commonjsPackageRoot = Files.toString(commonjsPackageRootFile, Charsets.UTF_8);

    // check are the URLs rewritten and point back to NX
    assertThat(commonjsPackageRoot, containsString(
        nexus().getUrl() + "content/repositories/" + REPO_ID + "/commonjs/-/commonjs-0.0.1.tgz"));

    // check that there are not traces of proxied registry URL
    assertThat(commonjsPackageRoot, not(containsString(mockRegistryServerUrl())));
  }
}
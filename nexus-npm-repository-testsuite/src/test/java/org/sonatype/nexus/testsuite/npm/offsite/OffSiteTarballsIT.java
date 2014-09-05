package org.sonatype.nexus.testsuite.npm.offsite;

import java.io.File;
import java.io.InputStreamReader;
import java.util.Map;
import java.util.concurrent.Callable;

import org.sonatype.nexus.testsuite.npm.MockNpmRegistry;
import org.sonatype.nexus.testsuite.npm.NpmITSupport;
import org.sonatype.nexus.testsuite.npm.smoke.NpmInstallIT;
import org.sonatype.sisu.litmus.testsupport.TestData;

import com.google.common.base.Charsets;
import com.google.common.collect.Maps;
import com.google.common.io.CharStreams;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsString;
import static org.hamcrest.Matchers.endsWith;
import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.hasSize;
import static org.hamcrest.Matchers.not;

/**
 * IT for off-site tarballs, ensuring that NX will pull tarballs for URL metadata says, and not from proxy registry's
 * remoteUrl + tarball path. This IT manages two mock registries, needs some handwork to make it working so
 * is not extending the NpmMockRegistryITSupport. This IT is basically doing very similar thing like the {@link
 * NpmInstallIT} except that here registry1 is being proxied, but all the tarballs are in registry2.
 */
public class OffSiteTarballsIT
    extends NpmITSupport
{
  private MockNpmRegistry mockNpmRegistry1;

  private MockNpmRegistry mockNpmRegistry2;

  public OffSiteTarballsIT(String nexusBundleCoordinates) {
    super(nexusBundleCoordinates);
  }

  /**
   * Starts mock NPM registries.
   *
   * @see TestData
   */
  @Before
  public void startMockNpmRegistryServer()
      throws Exception
  {
    // this registry contains tarballs
    mockNpmRegistry2 = new MockNpmRegistry(testData().resolveFile("registry2"), null).start();
    // this registry contains metadata, but all tarball URLs will point to registry2
    final Map<String, Callable<String>> properties = Maps.newHashMap();
    properties.put(MockNpmRegistry.TARBALL_BASE_URL, new Callable<String>()
    {
      @Override
      public String call() throws Exception {
        return mockNpmRegistry2.getUrl() + "/some/prefix";
      }
    });
    mockNpmRegistry1 = new MockNpmRegistry(testData().resolveFile("registry1"), properties).start();
  }

  /**
   * Stops mock NPM registries.
   *
   * @see #startMockNpmRegistryServer()
   */
  @After
  public void stopMockNpmRegistryServer()
      throws Exception
  {
    if (mockNpmRegistry1 != null) {
      mockNpmRegistry1.stop();
    }
    if (mockNpmRegistry2 != null) {
      mockNpmRegistry2.stop();
    }
  }

  @Test
  public void npmCliInstall() throws Exception {
    // create a NPM Proxy repository that proxies mock NPM registry
    createNpmProxyRepository(testMethodName(), mockNpmRegistry1.getUrl());

    final File localDirectory = util.createTempDir();
    final String cmd = String
        .format("npm install commonjs --registry %s --cache %s --prefix ./target/prefix --userconfig not-exists",
            nexus().getUrl().toExternalForm() + "content/repositories/" + testMethodName(),
            localDirectory.getAbsolutePath());

    log("CMD: {}", cmd);
    final Runtime rt = Runtime.getRuntime();
    final Process npm = rt.exec(cmd);
    final int exitCode = npm.waitFor();

    // Really no clue why npm CLI uses both for non-error output
    final String stdOut = CharStreams.toString(new InputStreamReader(npm.getInputStream(), Charsets.UTF_8));
    final String stdErr = CharStreams.toString(new InputStreamReader(npm.getErrorStream(), Charsets.UTF_8));

    log("STDRR:");
    log(stdErr);

    log("STDOUT:");
    log(stdOut);

    assertThat(stdErr, containsString(
        "npm http 200 " + nexus().getUrl() + "content/repositories/" + testMethodName() +
            "/commonjs/-/commonjs-0.0.1.tgz"));
    assertThat(stdErr, containsString(
        "npm http 200 " + nexus().getUrl() + "content/repositories/" + testMethodName() +
            "/system/-/system-0.1.0.tgz"));
    assertThat(stdErr, containsString(
        "npm http 200 " + nexus().getUrl() + "content/repositories/" + testMethodName() +
            "/test/-/test-0.6.0.tgz"));
    assertThat(stdErr, containsString(
        "npm http 200 " + nexus().getUrl() + "content/repositories/" + testMethodName() +
            "/ansi-font/-/ansi-font-0.0.2.tgz"));

    assertThat(stdOut, containsString("commonjs@0.0.1"));
    assertThat(stdOut, containsString("system@0.1.0"));
    assertThat(stdOut, containsString("test@0.6.0 (ansi-font@0.0.2)"));

    assertThat(exitCode, equalTo(0));

    // registry1 should never been asked for tarball
    for (String requestPath : mockNpmRegistry1.getPathRecorder().getPathsForVerb("GET")) {
      assertThat(requestPath, not(endsWith(".tgz")));
    }
    // registry2 should be asked for 4 tarballs only
    assertThat(mockNpmRegistry2.getPathRecorder().getPathsForVerb("GET"), hasSize(4));
    for (String requestPath : mockNpmRegistry2.getPathRecorder().getPathsForVerb("GET")) {
      assertThat(requestPath, endsWith(".tgz"));
    }
  }
}
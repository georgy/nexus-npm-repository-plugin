package org.sonatype.nexus.testsuite.npm.smoke;

import java.io.File;
import java.io.InputStreamReader;

import org.sonatype.nexus.testsuite.npm.NpmMockRegistryITSupport;

import com.google.common.base.Charsets;
import com.google.common.io.CharStreams;
import org.junit.Test;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsString;
import static org.hamcrest.Matchers.equalTo;

/**
 * NPM CLI IT for NPM plugin with simple "install" invocation. The test required the "npm" command to be present on
 * path. It performs an installation of "commonjs" package, with all of it dependencies. Assertion is made that
 * all the needed versions are fetched (versions present in mock NPM repository) from NX, and that NPM command exited
 * with success.
 *
 * This IT requires the "npm" command be present on path. Also, no clue how this IT would behave on Windows, as it's
 * tested on OSX and would probably also work on Linux.
 */
public class NpmInstallIT
    extends NpmMockRegistryITSupport
{
  public NpmInstallIT(String nexusBundleCoordinates) {
    super(nexusBundleCoordinates);
  }

  @Test
  public void npmCliInstall() throws Exception {
    // create a NPM Proxy repository that proxies mock NPM registry
    createNpmProxyRepository(testMethodName());

    final File localDirectory = util.createTempDir();
    final File projectDir = util.createTempDir();
    final String cmd = String
        .format("npm install commonjs --registry %s --cache %s --prefix %s --userconfig not-exists",
            nexus().getUrl().toExternalForm() + "content/repositories/" + testMethodName(),
            localDirectory.getAbsolutePath(), projectDir.getAbsolutePath());

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

    assertThat(stdOut, containsString("commonjs@0.0.1"));
    assertThat(stdOut, containsString("system@0.1.0"));
    assertThat(stdOut, containsString("test@0.6.0 (ansi-font@0.0.2)"));

    assertThat(exitCode, equalTo(0));
  }
}
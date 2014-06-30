package org.sonatype.nexus.testsuite.npm;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.Collection;
import java.util.Map;

import javax.annotation.Nullable;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.sonatype.nexus.bundle.launcher.NexusBundleConfiguration;
import org.sonatype.nexus.client.core.subsystem.content.Content;
import org.sonatype.nexus.testsuite.support.NexusRunningParametrizedITSupport;
import org.sonatype.nexus.testsuite.support.NexusStartAndStopStrategy;
import org.sonatype.tests.http.server.api.Behaviour;
import org.sonatype.tests.http.server.fluent.Server;

import com.google.common.base.Charsets;
import com.google.common.base.Strings;
import com.google.common.io.ByteStreams;
import com.google.common.io.Files;
import com.google.common.primitives.Ints;
import org.junit.After;
import org.junit.Before;
import org.junit.Rule;
import org.junit.rules.ExpectedException;
import org.junit.runners.Parameterized;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

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

  private Server server;

  @Before
  public void startMockRegistryServer()
      throws Exception
  {
    final File registryRoot = testData().resolveFile("registry");
    if (registryRoot.isDirectory()) {
      log("Starting mock NPM registry with root {}", registryRoot);
      server = Server.withPort(0).serve("/*").withBehaviours(new NpmGet(registryRoot)).start();
    }
  }

  @After
  public void stopMockRegistryServer()
      throws Exception
  {
    if (server != null) {
      server.stop();
    }
  }

  /**
   * If mock NPM registry is started, returns it's root URL as String, {@code null} otherwise.
   */
  @Nullable
  public String mockRegistryServerUrl() {
    if (server == null) {
      return null;
    }
    return "http://localhost:" + server.getPort();
  }

  public Content content() {
    return client().getSubsystem(Content.class);
  }

  // ==

  /**
   * A mock NPM GET behaviour (handle HTTP GETs only). It basically serves up files from it's supplied root
   * directory, with one trick: of the incoming request path maps to a directory, it will look for a file
   * called {@code data.json} in that directory and serve that instead. This allows one to lay down a directory
   * that resembles NPM registry.
   */
  public static class NpmGet
      implements Behaviour
  {
    private final Logger log = LoggerFactory.getLogger(NpmGet.class);

    private final File root;

    public NpmGet(File root) {
      this.root = root;
    }

    public boolean execute(final HttpServletRequest request, final HttpServletResponse response,
                           final Map<Object, Object> ctx)
        throws Exception
    {
      if ("GET".equals(request.getMethod())) {
        log.info("Requested {} {}", request.getMethod(), request.getPathInfo());
        final File file = new File(root, request.getPathInfo());
        if (file.isFile()) {
          log.info("Serving file {}", file.getAbsolutePath());
          sendFile(request, response, file);
          return false;
        }
        else if (file.isDirectory()) {
          final File metadata = new File(file, "data.json");
          if (metadata.isFile()) {
            log.info("Serving metadata {}", metadata.getAbsolutePath());
            sendMetadataFile(request, response, metadata);
            return false;
          }
        }
        response.sendError(404);
        return false;
      }

      return true;
    }

    private String baseUrl(final HttpServletRequest request) {
      String requestUrl = request.getRequestURL().toString();
      String pathInfo = request.getPathInfo();
      if (!Strings.isNullOrEmpty(pathInfo)) {
        requestUrl = requestUrl.substring(0, requestUrl.length() - pathInfo.length());
      }

      String servletPath = request.getServletPath();
      if (!Strings.isNullOrEmpty(servletPath)) {
        requestUrl = requestUrl.substring(0, requestUrl.length() - servletPath.length());
      }
      return requestUrl;
    }

    private void sendFile(final HttpServletRequest request, final HttpServletResponse response, final File file)
        throws IOException
    {
      response.setContentType("application/octet-stream");
      response.setContentLength(Ints.checkedCast(file.length()));
      try (InputStream in = new FileInputStream(file); OutputStream out = response.getOutputStream()) {
        ByteStreams.copy(in, out);
      }
    }

    private void sendMetadataFile(final HttpServletRequest request, final HttpServletResponse response, final File file)
        throws IOException
    {
      response.setContentType("application/json");
      // very basic "interpolation" for now, if we need more, we can add it later
      final String jsonMetadata = Files.toString(file, Charsets.UTF_8).replace("${npmRegistryUrl}", baseUrl(request));
      response.setContentLength(jsonMetadata.length()); // file length might change due to replacements above
      try (OutputStream out = response.getOutputStream()) {
        out.write(jsonMetadata.getBytes(Charsets.UTF_8));
        out.flush();
      }
    }
  }

}
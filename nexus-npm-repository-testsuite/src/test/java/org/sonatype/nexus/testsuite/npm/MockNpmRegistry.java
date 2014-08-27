package org.sonatype.nexus.testsuite.npm;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.Map;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.sonatype.tests.http.server.api.Behaviour;
import org.sonatype.tests.http.server.fluent.Server;
import org.sonatype.tests.http.server.jetty.behaviour.PathRecorderBehaviour;

import com.google.common.base.Charsets;
import com.google.common.base.Strings;
import com.google.common.base.Throwables;
import com.google.common.io.ByteStreams;
import com.google.common.io.Files;
import com.google.common.primitives.Ints;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import static com.google.common.base.Preconditions.checkArgument;
import static com.google.common.base.Preconditions.checkNotNull;
import static com.google.common.base.Preconditions.checkState;

/**
 * A mock NPM registry.
 */
public class MockNpmRegistry
{
  private final Logger logger = LoggerFactory.getLogger(MockNpmRegistry.class);

  private final File registryRoot;

  private Server server;

  private PathRecorderBehaviour pathRecorderBehaviour;

  /**
   * Constructor that access an existing directory as input.
   */
  public MockNpmRegistry(final File registryRoot) {
    this.registryRoot = checkNotNull(registryRoot);
    checkArgument(registryRoot.isDirectory(),
        "The registry root must point to existing directory (does not exists or is not directory)");
  }

  /**
   * Starts up the registry. Might be invoked only when registry was not already started or was stopped.
   */
  public synchronized MockNpmRegistry start() {
    checkState(server == null, "Server already started");
    try {
      pathRecorderBehaviour = new PathRecorderBehaviour();
      server = Server.withPort(0).serve("/*").withBehaviours(pathRecorderBehaviour, new NpmGet(registryRoot)).start();
      logger.info("Starting mock NPM registry with root {} at {}", registryRoot, getUrl());
      return this;
    }
    catch (Exception e) {
      throw Throwables.propagate(e);
    }
  }

  /**
   * Stops the registry. Might be invoked only when registry was started.
   */
  public synchronized MockNpmRegistry stop() {
    checkState(server != null, "Server not started");
    try {
      logger.info("Stopping mock NPM registry");
      server.stop();
      server = null;
      return this;
    }
    catch (Exception e) {
      throw Throwables.propagate(e);
    }
  }

  /**
   * Returns the URL of the registry. Might be invoked only if registry is running. The URL changes between
   * invocations, so caching return value makes sense only during one single "start" of registry.
   */
  public synchronized String getUrl() {
    checkState(server != null, "Server not started");
    return "http://localhost:" + server.getPort();
  }

  /**
   * Exposes path recorder to perform assertions. Returns {@code null} if mock registry never started. Is
   * re-initialized at each start. Meaning, if you start and then stop registry, you can still inspect recorded requests
   * from last run.
   */
  public PathRecorderBehaviour getPathRecorder() {
    return pathRecorderBehaviour;
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

    @Override
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
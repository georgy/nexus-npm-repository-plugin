package com.bolyuba.nexus.plugin.npm.service.tarball.internal;

import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.security.DigestOutputStream;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.Set;

import javax.inject.Inject;
import javax.inject.Named;
import javax.inject.Singleton;

import org.sonatype.nexus.apachehttpclient.Hc4Provider;
import org.sonatype.nexus.util.DigesterUtils;
import org.sonatype.sisu.goodies.common.ComponentSupport;

import com.bolyuba.nexus.plugin.npm.NpmRepository;
import com.bolyuba.nexus.plugin.npm.proxy.NpmProxyRepository;
import com.bolyuba.nexus.plugin.npm.service.PackageVersion;
import com.bolyuba.nexus.plugin.npm.service.tarball.Tarball;
import com.google.common.base.Throwables;
import com.google.common.collect.ImmutableSet;
import org.apache.http.HttpResponse;
import org.apache.http.HttpStatus;
import org.apache.http.client.HttpClient;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.client.protocol.HttpClientContext;
import org.apache.http.util.EntityUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import static com.google.common.base.Preconditions.checkNotNull;

/**
 * Apache HttpClient backed tarball transport implementation. It merely performs a HTTP GET and calculates response
 * body's SHA1 hash. No retries happen here (except of those at protocol level done by Apache HttpClient itself).
 */
@Singleton
@Named
public class HttpTarballTransport
    extends ComponentSupport
{
  /**
   * Using same log category as remote storage does, to simplify log reading.
   */
  private static final Logger outboundRequestLog = LoggerFactory.getLogger("remote.storage.outbound");

  /**
   * Set of HTTP response codes we do NOT want retries to happen:
   * <ul>
   * <li>404 - remote does not have it</li>
   * <li>401, 403 - probably user misconfiguration (remote authc needed or wrong)</li>
   * </ul>
   */
  private static final Set<Integer> NO_RETRIES_RESPONSE_CODES = ImmutableSet.of(404, 401, 403);

  private final Hc4Provider hc4Provider;

  @Inject
  public HttpTarballTransport(final Hc4Provider hc4Provider) {
    this.hc4Provider = checkNotNull(hc4Provider);
  }

  public Tarball getTarballForVersion(final NpmProxyRepository npmProxyRepository, final File target,
                                      final PackageVersion packageVersion)
      throws IOException
  {
    final HttpClient httpClient = hc4Provider.createHttpClient(npmProxyRepository.getRemoteStorageContext());
    final HttpGet get = new HttpGet(packageVersion.getDistTarball());
    outboundRequestLog.debug("{} - NPMTarball GET {}", npmProxyRepository.getId(), get.getURI());
    final HttpClientContext context = new HttpClientContext();
    context.setAttribute(Hc4Provider.HTTP_CTX_KEY_REPOSITORY, npmProxyRepository);
    get.addHeader("Accept", NpmRepository.TARBALL_MIME_TYPE);
    final HttpResponse httpResponse = httpClient.execute(get, context);
    try {
      outboundRequestLog.debug("{} - NPMTarball GET {} - {}", npmProxyRepository.getId(), get.getURI(),
          httpResponse.getStatusLine());
      if (httpResponse.getStatusLine().getStatusCode() == HttpStatus.SC_OK && httpResponse.getEntity() != null) {
        final MessageDigest md = MessageDigest.getInstance("SHA1");
        try (final BufferedOutputStream bos = new BufferedOutputStream(
            new DigestOutputStream(new FileOutputStream(target), md))) {
          httpResponse.getEntity().writeTo(bos);
          bos.flush();
        }
        return new Tarball(target, packageVersion.getDistTarballFilename(), get.getURI().toString(),
            DigesterUtils.getDigestAsString(md.digest()));
      }
      else {
        if (NO_RETRIES_RESPONSE_CODES.contains(httpResponse.getStatusLine().getStatusCode())) {
          log.debug("{} - NPMTarball GET {}: unexpected response: {}", npmProxyRepository.getId(), get.getURI(),
              httpResponse.getStatusLine());
          return null;
        }
        else {
          // in my experience, registry my throw many different errors, from 400, to 500 and then on next try
          // will happily serve the same URL! Hence, we do retry almost all of the responses that are not expected.
          log.debug("{} - NPMTarball GET {}: unexpected response: {}", npmProxyRepository.getId(), get.getURI(),
              httpResponse.getStatusLine());
          throw new IOException("Unexpected response for 'GET " + get.getURI() + "': " + httpResponse.getStatusLine());
        }
      }
    }
    catch (NoSuchAlgorithmException e) {
      throw Throwables.propagate(e);
    }
    finally {
      EntityUtils.consumeQuietly(httpResponse.getEntity());
    }
  }
}

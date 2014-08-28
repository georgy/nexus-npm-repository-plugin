package com.bolyuba.nexus.plugin.npm.service.tarball.internal;

import java.io.File;
import java.io.IOException;
import java.util.Map;

import javax.inject.Inject;
import javax.inject.Named;
import javax.inject.Singleton;

import org.sonatype.nexus.configuration.application.ApplicationDirectories;
import org.sonatype.nexus.proxy.repository.RemoteConnectionSettings;
import org.sonatype.sisu.goodies.common.ComponentSupport;

import com.bolyuba.nexus.plugin.npm.proxy.NpmProxyRepository;
import com.bolyuba.nexus.plugin.npm.service.tarball.Tarball;
import com.bolyuba.nexus.plugin.npm.service.tarball.TarballRequest;
import com.bolyuba.nexus.plugin.npm.service.tarball.TarballSource;
import com.bolyuba.nexus.plugin.npm.service.tarball.internal.TarballValidator.Result;

import static com.google.common.base.Preconditions.checkNotNull;

/**
 * Default {@link TarballSource} implementation.
 */
@Singleton
@Named
public class TarballSourceImpl
    extends ComponentSupport
    implements TarballSource
{
  private final ApplicationDirectories applicationDirectories;

  private final HttpTarballTransport tarballTransport;

  private final Map<String, TarballValidator> validators;

  @Inject
  public TarballSourceImpl(final ApplicationDirectories applicationDirectories,
                           final HttpTarballTransport tarballTransport,
                           final Map<String, TarballValidator> validators)
  {
    this.applicationDirectories = checkNotNull(applicationDirectories);
    this.tarballTransport = checkNotNull(tarballTransport);
    this.validators = checkNotNull(validators);
  }

  @Override
  public Tarball get(final NpmProxyRepository npmProxyRepository, final TarballRequest tarballRequest)
      throws IOException
  {
    int fetchRetries = 1;
    if (npmProxyRepository.getRemoteStorageContext() != null) {
      RemoteConnectionSettings settings = npmProxyRepository.getRemoteStorageContext().getRemoteConnectionSettings();
      if (settings != null) {
        fetchRetries = settings.getRetrievalRetryCount();
      }
    }
    final File tempFile = File
        .createTempFile(npmProxyRepository.getId() + "-tarball", "tgz",
            applicationDirectories.getTemporaryDirectory());
    for (int i = 0; i < fetchRetries; i++) {
      log.debug("Retry {}/{} for {}@{} tarball...", i, fetchRetries, tarballRequest.getPackageVersion().getName(),
          tarballRequest.getPackageVersion().getVersion());
      try {
        Tarball tarball = tarballTransport
            .getTarballForVersion(npmProxyRepository, tempFile, tarballRequest.getPackageVersion());
        if (tarball == null) {
          log.debug("Tarball for {}@{} not found on {}",
              tarballRequest.getPackageVersion().getName(),
              tarballRequest.getPackageVersion().getVersion(),
              tarballRequest.getPackageVersion().getDistTarball());
          return null;
        }
        for (TarballValidator validator : validators.values()) {
          final Result result = validator.validate(tarballRequest, tarball);
          if (log.isDebugEnabled()) { // lot of acrobatics, better guard it
            log.debug("Validated tarball {}@{} :: {} found '{}' by validator {}",
                tarballRequest.getPackageVersion().getName(),
                tarballRequest.getPackageVersion().getVersion(),
                tarball.getOriginUrl(),
                result.name(),
                validator.getClass().getSimpleName());
          }
          if (result == Result.INVALID) {
            tarball.delete();
            throw new IOException("Invalid content detected: " + validator.getClass().getSimpleName());
          }
        }
        return tarball;
      }
      catch (IOException e) {
        // note and retry
        log.warn("Fetch {}/{} failed for {}@{} tarball: {}",
            i, fetchRetries,
            tarballRequest.getPackageVersion().getName(),
            tarballRequest.getPackageVersion().getVersion(),
            String.valueOf(e));
      }
    }
    return null;
  }
}

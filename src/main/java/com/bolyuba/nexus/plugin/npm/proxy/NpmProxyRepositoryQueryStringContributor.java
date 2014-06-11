package com.bolyuba.nexus.plugin.npm.proxy;

import javax.inject.Inject;
import javax.inject.Named;
import javax.inject.Singleton;
import javax.servlet.http.HttpServletRequest;

import org.sonatype.nexus.proxy.repository.ProxyRepository;
import org.sonatype.nexus.proxy.storage.remote.RemoteStorageContext;
import org.sonatype.nexus.proxy.storage.remote.http.QueryStringContributor;
import org.sonatype.sisu.goodies.common.ComponentSupport;

import com.bolyuba.nexus.plugin.npm.NpmContentClass;
import com.bolyuba.nexus.plugin.npm.NpmUtility;
import com.google.inject.Provider;

import static com.google.common.base.Preconditions.checkNotNull;

/**
 * Query string contributor to pass over the NPM client query string, if needed.
 */
@Singleton
@Named
public class NpmProxyRepositoryQueryStringContributor
    extends ComponentSupport
    implements QueryStringContributor
{
  private final Provider<HttpServletRequest> httpServletRequestProvider;

  private final NpmUtility npmUtility;

  @Inject
  public NpmProxyRepositoryQueryStringContributor(final Provider<HttpServletRequest> httpServletRequestProvider,
                                                  final NpmUtility npmUtility)
  {
    this.httpServletRequestProvider = checkNotNull(httpServletRequestProvider);
    this.npmUtility = checkNotNull(npmUtility);
  }

  /**
   * This method will provide query string only if called within a context of a HTTP Servlet Request, and
   * if that request is detected as NPM client call.
   */
  @Override
  public String getQueryString(final RemoteStorageContext ctx, final ProxyRepository repository) {
    if (NpmContentClass.ID.equals(repository.getRepositoryContentClass().getId())) {
      final HttpServletRequest httpServletRequest = httpServletRequestProvider.get();
      if (httpServletRequest != null && npmUtility.isNmpRequest(httpServletRequest)) {
        return httpServletRequest.getQueryString();
      }
    }
    return null;
  }
}

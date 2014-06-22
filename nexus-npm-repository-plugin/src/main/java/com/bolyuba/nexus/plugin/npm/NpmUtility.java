package com.bolyuba.nexus.plugin.npm;

import com.bolyuba.nexus.plugin.npm.pkg.PackageRequest;
import com.google.inject.Provider;
import com.google.inject.ProvisionException;
import org.sonatype.nexus.proxy.RequestContext;
import org.sonatype.nexus.proxy.ResourceStoreRequest;
import org.sonatype.nexus.proxy.item.RepositoryItemUid;

import javax.annotation.Nonnull;
import javax.inject.Inject;
import javax.inject.Named;
import javax.inject.Singleton;
import javax.servlet.http.HttpServletRequest;

/**
 * Utility class that implements most of the commonjs/npm related plumbing.
 *
 * @author Georgy Bolyuba (georgy@bolyuba.com)
 */
@Named
@Singleton
public class NpmUtility {

    final Provider<HttpServletRequest> httpServletRequestProvider;

    @Inject
    public NpmUtility(
            @SuppressWarnings("CdiInjectionPointsInspection") final Provider<HttpServletRequest> httpServletRequestProvider) {
        this.httpServletRequestProvider = httpServletRequestProvider;
    }

    /**
     * Trying to decide if request is coming form npm utility. Works closely with
     * {@link com.bolyuba.nexus.plugin.npm.NpmUtility#isNmpRequest(javax.servlet.http.HttpServletRequest)}
     *
     * @param request request we are about to process
     * @return {@code true} if we think request is coming form npm utility, {@code false} otherwise (for example,
     * if someone is browsing content of the repo in Nexus UI).
     */
    public boolean isNmpRequest(@SuppressWarnings("UnusedParameters") ResourceStoreRequest request) {

        if (request.getRequestContext().containsKey(RequestContext.CTX_AUTH_CHECK_ONLY)) {
            // not a real request, authentication check. We are not interested
            return false;
        }

        try {
            return isNmpRequest(httpServletRequestProvider.get());
        } catch (ProvisionException e) {
            // this means the call happened outside of a scope of HTTP request
            return false;
        }
    }

    /**
     * Trying to decide if request is coming form npm utility.
     * <p/>
     * Following http://wiki.commonjs.org/wiki/Packages/Registry#HTTP_Request_Method_and_Headers
     * checking Accept for "application/json".
     *
     * @param httpServletRequest HTTP Servlet request we are about to process
     * @return {@code true} if we think request is coming form npm utility, {@code false} otherwise (for example,
     * if someone is browsing content of the repo in Nexus UI).
     */
    public boolean isNmpRequest(final HttpServletRequest httpServletRequest) {
        if (httpServletRequest == null) {
            throw new IllegalStateException("Container did not provide an instance of HttpServletRequest");
        }

        String accept = httpServletRequest.getHeader("accept");

        return accept != null && accept.toLowerCase().equals(NpmRepository.JSON_MIME_TYPE);
    }

    public ResourceStoreRequest wrapRequest(@Nonnull ResourceStoreRequest request) {
        String path = getRequestContentCachePath(request.getRequestPath());
        request.setRequestPath(path);
        return request;
    }

    public ResourceStoreRequest replaceRequest(@Nonnull ResourceStoreRequest request) {
        String path = getRequestContentCachePath(request.getRequestPath());
        return new ResourceStoreRequest(path);
    }

    public ResourceStoreRequest createRequest(@Nonnull PackageRequest packageRequest) {
        String path = getRequestContentCachePath(packageRequest.getPath());
        return new ResourceStoreRequest(path);
    }

    String getRequestContentCachePath(@Nonnull String path) {
        if (!path.endsWith(RepositoryItemUid.PATH_SEPARATOR)) {
            path = path + RepositoryItemUid.PATH_SEPARATOR;
        }
        return path + NpmRepository.JSON_CONTENT_FILE_NAME;
    }
}
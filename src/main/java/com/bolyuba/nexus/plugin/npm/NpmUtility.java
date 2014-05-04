package com.bolyuba.nexus.plugin.npm;

import com.google.inject.Provider;
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
     * Trying to decide if request is coming form npm utility.
     * <p/>
     * Following http://wiki.commonjs.org/wiki/Packages/Registry#HTTP_Request_Method_and_Headers
     * checking Accept for "application/json" would be a good idea. Right now it is not possible as
     * {@link org.sonatype.nexus.web.content.NexusContentServlet#getResourceStoreRequest(javax.servlet.http.HttpServletRequest)}
     * does not map Accept header into anything.
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

        HttpServletRequest httpServletRequest = httpServletRequestProvider.get();
        if (httpServletRequest == null) {
            return false;
        }

        String accept = httpServletRequest.getHeader("accept");

        return accept != null && accept.toLowerCase().equals(NpmRepository.JSON_MIME_TYPE);
    }

    public ResourceStoreRequest wrapRequest(ResourceStoreRequest request) {
        String path = getRequestContentCachePath(request.getRequestPath());
        request.setRequestPath(path);
        return request;
    }

    public ResourceStoreRequest replaceRequest(ResourceStoreRequest request) {
        String path = getRequestContentCachePath(request.getRequestPath());
        return new ResourceStoreRequest(path);
    }

    String getRequestContentCachePath(@Nonnull String path) {
        if (!path.endsWith(RepositoryItemUid.PATH_SEPARATOR)) {
            path = path + RepositoryItemUid.PATH_SEPARATOR;
        }
        return path + NpmRepository.JSON_CONTENT_FILE_NAME;
    }
}
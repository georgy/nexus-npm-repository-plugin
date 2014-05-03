package com.bolyuba.nexus.plugin.npm;

import com.bolyuba.nexus.plugin.npm.hosted.NpmHostedRepository;
import com.bolyuba.nexus.plugin.npm.hosted.content.NpmJsonContentLocator;
import com.bolyuba.nexus.plugin.npm.pkg.PackageRequest;
import com.bolyuba.nexus.plugin.npm.proxy.content.NpmFilteringContentLocator;
import com.google.gson.Gson;
import com.google.gson.stream.JsonReader;
import com.google.gson.stream.JsonToken;
import com.google.inject.Provider;
import org.apache.commons.codec.binary.Base64InputStream;
import org.sonatype.nexus.proxy.AccessDeniedException;
import org.sonatype.nexus.proxy.IllegalOperationException;
import org.sonatype.nexus.proxy.ItemNotFoundException;
import org.sonatype.nexus.proxy.LocalStorageException;
import org.sonatype.nexus.proxy.RequestContext;
import org.sonatype.nexus.proxy.ResourceStoreRequest;
import org.sonatype.nexus.proxy.item.DefaultStorageFileItem;
import org.sonatype.nexus.proxy.item.RepositoryItemUid;
import org.sonatype.nexus.proxy.repository.ProxyRepository;
import org.sonatype.nexus.proxy.storage.UnsupportedStorageOperationException;
import org.sonatype.nexus.proxy.storage.local.LocalRepositoryStorage;

import javax.annotation.Nonnull;
import javax.inject.Inject;
import javax.inject.Named;
import javax.inject.Singleton;
import javax.servlet.http.HttpServletRequest;
import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.HashMap;
import java.util.Map;

/**
 * Utility class that implements most of the commonjs/npm related plumbing.
 *
 * @author Georgy Bolyuba (georgy@bolyuba.com)
 */
@Named
@Singleton
public class NpmUtility {

    static final String NPM_DECORATED_FLAG = "npm.decorated";

    static final String JSON_CONTENT_FILE_NAME = "content.json";

    static final String JSON_MIME_TYPE = "application/json";

    static final String HIDDEN_CACHE_PREFIX = RepositoryItemUid.PATH_SEPARATOR + ".cache";

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
    public final boolean isNmpRequest(@SuppressWarnings("UnusedParameters") ResourceStoreRequest request) {

        HttpServletRequest httpServletRequest = httpServletRequestProvider.get();
        if (httpServletRequest == null) {
            return false;
        }

        String accept = httpServletRequest.getHeader("accept");

        return accept != null && accept.toLowerCase().equals(JSON_MIME_TYPE);
    }

    public final boolean isTarballRequest(@SuppressWarnings("UnusedParameters") ResourceStoreRequest request) {
        return request.getRequestPath().toLowerCase().endsWith(".tgz");
    }

    public final String suggestMimeType(@Nonnull String path) {
        // this should take into account if request in from npm or not
        // right now we only know that content.json is json
        if (path.toLowerCase().endsWith(JSON_CONTENT_FILE_NAME)) {
            return JSON_MIME_TYPE;
        }
        return null;
    }

    public DefaultStorageFileItem wrapJsonItem(ProxyRepository repository, DefaultStorageFileItem item) {
        ResourceStoreRequest request = item.getResourceStoreRequest();
        NpmFilteringContentLocator decoratedContentLocator = decorateContentLocator(item, request, repository.getRemoteUrl());
        ResourceStoreRequest decoratedRequest = decorateRequest(request);

        DefaultStorageFileItem storageFileItem = new DefaultStorageFileItem(
                repository,
                decoratedRequest,
                item.isReadable(),
                item.isWritable(),
                decoratedContentLocator);

        storageFileItem.getItemContext().put(NPM_DECORATED_FLAG, true);
        return storageFileItem;
    }

    private NpmFilteringContentLocator decorateContentLocator(DefaultStorageFileItem item, ResourceStoreRequest request, @Nonnull String remoteUrl) {
        return new NpmFilteringContentLocator(item.getContentLocator(), request, remoteUrl);
    }

    private ResourceStoreRequest decorateRequest(ResourceStoreRequest request) {
        String path = request.getRequestPath();
        if (!path.endsWith(RepositoryItemUid.PATH_SEPARATOR)) {
            path = path + RepositoryItemUid.PATH_SEPARATOR;
        }
        request.setRequestPath(path + JSON_CONTENT_FILE_NAME);
        return request;
    }

    static final String NPM_PACKAGE = "npm.package";

    static final String NPM_VERSION = "npm.version";

    /**
     * Adds npm metadata to the request context
     *
     * @param request request we want to decorate
     */
    public void addNpmMeta(@Nonnull ResourceStoreRequest request) {
        String requestPath = request.getRequestPath();
        if (requestPath == null) {
            // wtf?
            return;
        }

        if (RepositoryItemUid.PATH_SEPARATOR.equals(requestPath)) {
            return;
        }

        RequestContext context = request.getRequestContext();

        String correctedPath =
                requestPath.startsWith(RepositoryItemUid.PATH_SEPARATOR) ?
                        requestPath.substring(1, requestPath.length()) :
                        requestPath;

        String[] explodedPath = correctedPath.split(RepositoryItemUid.PATH_SEPARATOR);

        if (explodedPath.length >= 1) {
            context.put(NPM_PACKAGE, explodedPath[0]);
        }

        if (explodedPath.length >= 2) {
            context.put(NPM_VERSION, explodedPath[1]);
        }
    }



    public ResourceStoreRequest hideInCache(ResourceStoreRequest request) {
        request.setRequestPath(HIDDEN_CACHE_PREFIX + request.getRequestPath());
        return request;
    }

    public void processStoreRequest(@Nonnull DefaultStorageFileItem hiddenItem, @Nonnull NpmHostedRepository repository) throws LocalStorageException, UnsupportedStorageOperationException,
            IllegalOperationException, AccessDeniedException, ItemNotFoundException {
        String path = hiddenItem.getPath();

        if ((path == null) || (!path.startsWith(HIDDEN_CACHE_PREFIX))) {
            throw new LocalStorageException("Something went wrong. Publish request was not saved in " + HIDDEN_CACHE_PREFIX);
        }

        // get to real package root
        String packageRoot = path.substring(HIDDEN_CACHE_PREFIX.length(), path.length());

        Gson gson = new Gson();
        try {
            Versions box = gson.fromJson(new InputStreamReader(hiddenItem.getInputStream()), Versions.class);

            if ((box == null) || (box.versions == null) || (box.versions.isEmpty())) {
                throw new LocalStorageException("Unable to extract versions from cached publish request");
            }
            for (String version : box.versions.keySet()) {
                Object obj = box.versions.get(version);
                processVersion(packageRoot, version, gson.toJson(obj), repository);
                String tarball = isPublishingTarball(obj);

                if (tarball != null) {
                    ResourceStoreRequest request = new ResourceStoreRequest("/.cache/z-my-test-app-0.0.5.tgz");
                    InputStream inputStream = getInputStream(hiddenItem, "z-my-test-app-0.0.5.tgz");
                    if (inputStream == null) {
                        throw new LocalStorageException("Was not able to get InputStream for tarball from cached publish request");
                    }
                    repository.storeItem(request, inputStream, null);
                }
            }
        } catch (IOException e) {
            throw new LocalStorageException(e);
        }

    }

    private String isPublishingTarball(Object version) {
        if (!Map.class.isInstance(version)) {
            return null;
        }

        Map map = (Map) version;
        if (!map.containsKey("dist")) {
            return null;
        }

        if (!Map.class.isInstance(map.get("dist"))) {
            return null;
        }

        Map dist = (Map) map.get("dist");

        if (dist.containsKey("tarball")) {
            return (String) dist.get("tarball");
        }

        return null;
    }

    private InputStream getInputStream(DefaultStorageFileItem item, String tarballName) throws IOException {
        JsonReader jsonReader = new JsonReader(new InputStreamReader(item.getInputStream()));

        if (!skipToName(jsonReader, "_attachments")) {
            return null;
        }

        if (!skipToName(jsonReader, tarballName)) {
            return null;
        }

        if (!skipToName(jsonReader, "data")) {
            return null;
        }
        Base64InputStream result = new Base64InputStream(new ByteArrayInputStream(jsonReader.nextString().getBytes()));
        jsonReader.close();
        return result;
    }

    private boolean skipToName(JsonReader jsonReader, String targetName) throws IOException {

        do {
            JsonToken peek = jsonReader.peek();

            if (peek == JsonToken.BEGIN_OBJECT) {
                jsonReader.beginObject();
                continue;
            }

            if (peek == JsonToken.END_DOCUMENT) {
                return false;
            }

            if (peek != JsonToken.NAME) {
                jsonReader.skipValue();
                continue;
            }

            String name = jsonReader.nextName();

            if (targetName.equals(name.toLowerCase())) {
                return true;
            } else {
                jsonReader.skipValue();
            }
        } while (true);
    }


    private void processVersion(String packageRoot, String version, String json, NpmHostedRepository repository) throws UnsupportedStorageOperationException, LocalStorageException {
        LocalRepositoryStorage localStorage = repository.getLocalStorage();

        ResourceStoreRequest resourceStoreRequest = new ResourceStoreRequest(packageRoot + RepositoryItemUid.PATH_SEPARATOR + version, true, false);
        ResourceStoreRequest decoratedRequest = decorateRequest(resourceStoreRequest);

        DefaultStorageFileItem item = new DefaultStorageFileItem(repository,
                decoratedRequest,
                true,
                true,
                new NpmJsonContentLocator(json));

        localStorage.storeItem(repository, item);
    }

    /**
     * For given package request's content get real storage request. We are mapping json REST-like API onto
     * filesystem.
     *
     * @param packageRequest request in question
     * @return storage request for content of package request
     */
    public ResourceStoreRequest getContentStorageRequest(@Nonnull PackageRequest packageRequest) {
        ResourceStoreRequest storeRequest = packageRequest.getStoreRequest();

        String path = storeRequest.getRequestPath();
        if (!path.endsWith(RepositoryItemUid.PATH_SEPARATOR)) {
            path = path + RepositoryItemUid.PATH_SEPARATOR;
        }

        return new ResourceStoreRequest(path + JSON_CONTENT_FILE_NAME);
    }
}

class Versions {
    HashMap<String, Object> versions = new HashMap<>();
}

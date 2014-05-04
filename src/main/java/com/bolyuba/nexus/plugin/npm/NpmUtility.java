package com.bolyuba.nexus.plugin.npm;

import com.bolyuba.nexus.plugin.npm.hosted.NpmHostedRepository;
import com.bolyuba.nexus.plugin.npm.hosted.content.NpmJsonContentLocator;
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

    static final String JSON_CONTENT_FILE_NAME = "-content.json";

    static final String HIDDEN_CACHE_PREFIX = RepositoryItemUid.PATH_SEPARATOR + ".cache";

    final Provider<HttpServletRequest> httpServletRequestProvider;

    @Inject
    public NpmUtility(
            @SuppressWarnings("CdiInjectionPointsInspection") final Provider<HttpServletRequest> httpServletRequestProvider) {
        this.httpServletRequestProvider = httpServletRequestProvider;
    }

    private ResourceStoreRequest decorateRequest(ResourceStoreRequest request) {
        String path = request.getRequestPath();
        if (!path.endsWith(RepositoryItemUid.PATH_SEPARATOR)) {
            path = path + RepositoryItemUid.PATH_SEPARATOR;
        }
        request.setRequestPath(path + JSON_CONTENT_FILE_NAME);
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
}

class Versions {
    HashMap<String, Object> versions = new HashMap<>();
}

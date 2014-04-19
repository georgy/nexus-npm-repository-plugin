package com.bolyuba.nexus.plugin.npm.proxy;

import com.bolyuba.nexus.plugin.npm.proxy.content.NpmFilteringContentLocator;
import org.sonatype.nexus.proxy.RequestContext;
import org.sonatype.nexus.proxy.ResourceStoreRequest;
import org.sonatype.nexus.proxy.item.DefaultStorageFileItem;
import org.sonatype.nexus.proxy.item.RepositoryItemUid;
import org.sonatype.nexus.proxy.item.StorageItem;
import org.sonatype.nexus.proxy.repository.ProxyRepository;
import org.sonatype.nexus.proxy.repository.Repository;

import javax.annotation.Nonnull;
import javax.inject.Named;
import javax.inject.Singleton;

/**
 * @author Georgy Bolyuba (georgy@bolyuba.com)
 */
@Named
@Singleton
public class NpmUtility {

    private static final String NPM_DECORATED_FLAG = "npm.decorated";

    private static final String JSON_CONTENT_FILE_NAME = "content.json";

    private static final String JSON_MIME_TYPE = "application/json";

    public final boolean isNmpRequest(ResourceStoreRequest request) {
        RequestContext context = request.getRequestContext();
        if (context == null) {
            return false;
        }

        Object o = context.get("request.agent");
        if (o == null) {
            return false;
        }

        if (o.toString().toLowerCase().startsWith("node")) {
            return true;
        }
        return false;
    }

    public final String suggetMimeType(@Nonnull String path) {
        if (path.toLowerCase().endsWith(JSON_CONTENT_FILE_NAME)) {
            return JSON_MIME_TYPE;
        }
        return null;
    }

    public final boolean isJson(DefaultStorageFileItem item) {
        return JSON_MIME_TYPE.equals(item.getMimeType());
    }

    public final DefaultStorageFileItem decorateNpmJsonItem(ProxyRepository repository, ResourceStoreRequest request, DefaultStorageFileItem item) {
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

    public final boolean shouldNotGotRemote(ResourceStoreRequest request) {
        if (request.getRequestPath().toLowerCase().endsWith(JSON_CONTENT_FILE_NAME)) {
            return true;
        }
        return false;
    }

    public final boolean shouldNotCache(ResourceStoreRequest request) {
        // TODO: This does not work yet, always returns full "all"
        if ("/-/all/since".equals(request.getRequestPath())) {
            return true;
        }
        return false;
    }
}

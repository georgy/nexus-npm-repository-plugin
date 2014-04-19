package com.bolyuba.nexus.plugin.npm.proxy.storage;

import com.bolyuba.nexus.plugin.npm.proxy.NpmUtility;
import org.sonatype.nexus.proxy.ItemNotFoundException;
import org.sonatype.nexus.proxy.RemoteAccessException;
import org.sonatype.nexus.proxy.RemoteStorageException;
import org.sonatype.nexus.proxy.ResourceStoreRequest;
import org.sonatype.nexus.proxy.item.AbstractStorageItem;
import org.sonatype.nexus.proxy.item.DefaultStorageFileItem;
import org.sonatype.nexus.proxy.item.StorageItem;
import org.sonatype.nexus.proxy.repository.ProxyRepository;
import org.sonatype.nexus.proxy.storage.UnsupportedStorageOperationException;
import org.sonatype.nexus.proxy.storage.remote.RemoteRepositoryStorage;

import javax.annotation.Nonnull;
import java.net.URL;

/**
 * @author Georgy Bolyuba (georgy@bolyuba.com)
 */
public class NpmRemoteStorageWrapper
        implements RemoteRepositoryStorage {

    private RemoteRepositoryStorage realStorage;

    private NpmUtility utility;

    public NpmRemoteStorageWrapper(@Nonnull RemoteRepositoryStorage realStorage, @Nonnull NpmUtility utility) {
        this.realStorage = realStorage;
        this.utility = utility;
    }

    @Override
    public String getProviderId() {
        return realStorage.getProviderId();
    }

    @Override
    public String getVersion() {
        return realStorage.getVersion();
    }

    @Override
    public boolean isReachable(ProxyRepository repository, ResourceStoreRequest request) throws RemoteAccessException, RemoteStorageException {
        return realStorage.isReachable(repository, request);
    }

    @Override
    public URL getAbsoluteUrlFromBase(ProxyRepository repository, ResourceStoreRequest request) throws RemoteStorageException {
        return realStorage.getAbsoluteUrlFromBase(repository, request);
    }

    @Override
    public void validateStorageUrl(String url) throws RemoteStorageException {
        realStorage.validateStorageUrl(url);
    }

    @Override
    public boolean containsItem(ProxyRepository repository, ResourceStoreRequest request) throws RemoteAccessException, RemoteStorageException {
        return realStorage.containsItem(repository, request);
    }

    @Override
    public boolean containsItem(long newerThen, ProxyRepository repository, ResourceStoreRequest request) throws RemoteAccessException, RemoteStorageException {
        return realStorage.containsItem(newerThen, repository, request);
    }

    @Override
    public AbstractStorageItem retrieveItem(ProxyRepository repository, ResourceStoreRequest request, String baseUrl) throws ItemNotFoundException, RemoteAccessException, RemoteStorageException {
        DefaultStorageFileItem item = (DefaultStorageFileItem) realStorage.retrieveItem(repository, request, baseUrl);
        if (utility.isJson(item)) {
            return utility.decorateNpmJsonItem(repository, request, item);
        }
        return item;
    }

    @Override
    public void storeItem(ProxyRepository repository, StorageItem item) throws UnsupportedStorageOperationException, RemoteAccessException, RemoteStorageException {
        realStorage.storeItem(repository, item);
    }

    @Override
    public void deleteItem(ProxyRepository repository, ResourceStoreRequest request) throws ItemNotFoundException, UnsupportedStorageOperationException, RemoteAccessException, RemoteStorageException {
        realStorage.deleteItem(repository, request);
    }
}

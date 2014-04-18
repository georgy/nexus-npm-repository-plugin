package com.bolyuba.nexus.plugin.npm.proxy;

import org.sonatype.nexus.proxy.ItemNotFoundException;
import org.sonatype.nexus.proxy.LocalStorageException;
import org.sonatype.nexus.proxy.ResourceStoreIteratorRequest;
import org.sonatype.nexus.proxy.ResourceStoreRequest;
import org.sonatype.nexus.proxy.item.AbstractStorageItem;
import org.sonatype.nexus.proxy.item.StorageItem;
import org.sonatype.nexus.proxy.repository.Repository;
import org.sonatype.nexus.proxy.storage.UnsupportedStorageOperationException;
import org.sonatype.nexus.proxy.storage.local.LocalRepositoryStorage;

import javax.annotation.Nonnull;
import java.net.URL;
import java.util.Collection;
import java.util.Iterator;

/**
 * @author Georgy Bolyuba (georgy@bolyuba.com)
 */
public class NpmLocalStorageWrapper
        implements LocalRepositoryStorage {

    private final PathUtility pathUtility = new PathUtility();

    private LocalRepositoryStorage realStorage;

    public NpmLocalStorageWrapper(@Nonnull LocalRepositoryStorage realStorage) {
        this.realStorage = realStorage;
    }

    @Override
    public String getProviderId() {
        return realStorage.getProviderId();
    }

    @Override
    public void validateStorageUrl(String url) throws LocalStorageException {
        realStorage.validateStorageUrl(url);
    }

    @Override
    public boolean isReachable(Repository repository, ResourceStoreRequest request) throws LocalStorageException {
        return realStorage.isReachable(repository, request);
    }

    @Override
    public AbstractStorageItem retrieveItem(Repository repository, ResourceStoreRequest request) throws ItemNotFoundException, LocalStorageException {
        request.setRequestPath(pathUtility.fixLocalPath(request.getRequestPath()));
        return realStorage.retrieveItem(repository, request);
    }

    @Override
    public boolean containsItem(Repository repository, ResourceStoreRequest request) throws LocalStorageException {
        request.setRequestPath(pathUtility.fixLocalPath(request.getRequestPath()));
        return realStorage.containsItem(repository, request);
    }

    @Override
    public void storeItem(Repository repository, StorageItem item) throws UnsupportedStorageOperationException, LocalStorageException {
        realStorage.storeItem(repository, item);
    }

    @Override
    public void deleteItem(Repository repository, ResourceStoreRequest request) throws ItemNotFoundException, UnsupportedStorageOperationException, LocalStorageException {
        realStorage.deleteItem(repository, request);
    }

    @Override
    public void shredItem(Repository repository, ResourceStoreRequest request) throws ItemNotFoundException, UnsupportedStorageOperationException, LocalStorageException {
        realStorage.shredItem(repository, request);
    }

    @Override
    public void moveItem(Repository repository, ResourceStoreRequest from, ResourceStoreRequest to) throws ItemNotFoundException, UnsupportedStorageOperationException, LocalStorageException {
        realStorage.moveItem(repository, from, to);
    }

    @Override
    public Collection<StorageItem> listItems(Repository repository, ResourceStoreRequest request) throws ItemNotFoundException, LocalStorageException {
        return realStorage.listItems(repository, request);
    }

    @Override
    public Iterator<StorageItem> iterateItems(Repository repository, ResourceStoreIteratorRequest request) throws ItemNotFoundException, LocalStorageException {
        return realStorage.iterateItems(repository, request);
    }

    @Override
    public URL getAbsoluteUrlFromBase(Repository repository, ResourceStoreRequest request) throws LocalStorageException {
        return realStorage.getAbsoluteUrlFromBase(repository, request);
    }
}

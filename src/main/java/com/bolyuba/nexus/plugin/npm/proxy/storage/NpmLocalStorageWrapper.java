package com.bolyuba.nexus.plugin.npm.proxy.storage;

import com.bolyuba.nexus.plugin.npm.NpmUtility;
import org.sonatype.nexus.proxy.ItemNotFoundException;
import org.sonatype.nexus.proxy.LocalStorageException;
import org.sonatype.nexus.proxy.ResourceStoreIteratorRequest;
import org.sonatype.nexus.proxy.ResourceStoreRequest;
import org.sonatype.nexus.proxy.item.AbstractStorageItem;
import org.sonatype.nexus.proxy.item.StorageCollectionItem;
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

    private LocalRepositoryStorage realStorage;

    private NpmUtility utility;

    public NpmLocalStorageWrapper(@Nonnull LocalRepositoryStorage realStorage, @Nonnull NpmUtility utility) {
        this.realStorage = realStorage;
        this.utility = utility;
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
        AbstractStorageItem item = realStorage.retrieveItem(repository, request);

        // if request is from npm serve json content instead of collection
        if (StorageCollectionItem.class.isInstance(item) && utility.isNmpRequest(request)) {
            String path = item.getPath();
            if (!path.endsWith("/")) {
                path = path + "/";
            }
            ResourceStoreRequest contentRequest = new ResourceStoreRequest(path + "content.json");
            return realStorage.retrieveItem(repository, contentRequest);
        }

        return item;
    }


    @Override
    public boolean containsItem(Repository repository, ResourceStoreRequest request) throws LocalStorageException {
        boolean containsItem = realStorage.containsItem(repository, request);

        // if we do not have it, simply return
        if (!containsItem) {
            return false;
        }

        try {
            AbstractStorageItem item = this.retrieveItem(repository, request);
            // if request is from npm check if we have content and not just folder
            if (StorageCollectionItem.class.isInstance(item) && utility.isNmpRequest(request)) {
                String path = item.getPath();
                if (!path.endsWith("/")) {
                    path = path + "/";
                }
                ResourceStoreRequest contentRequest = new ResourceStoreRequest(path + "content.json");
                return realStorage.containsItem(repository, contentRequest);
            } else {
                return true;
            }
        } catch (ItemNotFoundException ignore) {
            // was deleted between two checks?
            return false;
        }
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

    @SuppressWarnings("deprecation")
    @Override
    public Iterator<StorageItem> iterateItems(Repository repository, ResourceStoreIteratorRequest request) throws ItemNotFoundException, LocalStorageException {
        return realStorage.iterateItems(repository, request);
    }

    @SuppressWarnings("deprecation")
    @Override
    public URL getAbsoluteUrlFromBase(Repository repository, ResourceStoreRequest request) throws LocalStorageException {
        return realStorage.getAbsoluteUrlFromBase(repository, request);
    }
}

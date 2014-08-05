package com.bolyuba.nexus.plugin.npm.proxy;

import java.io.IOException;

import com.bolyuba.nexus.plugin.npm.NpmContentClass;
import com.bolyuba.nexus.plugin.npm.NpmRepository;
import com.bolyuba.nexus.plugin.npm.content.NpmMimeRulesSource;
import com.bolyuba.nexus.plugin.npm.metadata.MetadataServiceFactory;
import com.bolyuba.nexus.plugin.npm.metadata.ProxyMetadataService;
import com.bolyuba.nexus.plugin.npm.pkg.PackageRequest;
import org.codehaus.plexus.util.xml.Xpp3Dom;
import org.sonatype.inject.Description;
import org.sonatype.nexus.configuration.Configurator;
import org.sonatype.nexus.configuration.model.CRepository;
import org.sonatype.nexus.configuration.model.CRepositoryExternalConfigurationHolderFactory;
import org.sonatype.nexus.mime.MimeRulesSource;
import org.sonatype.nexus.proxy.IllegalOperationException;
import org.sonatype.nexus.proxy.ItemNotFoundException;
import org.sonatype.nexus.proxy.LocalStorageException;
import org.sonatype.nexus.proxy.ResourceStoreRequest;
import org.sonatype.nexus.proxy.item.AbstractStorageItem;
import org.sonatype.nexus.proxy.item.ContentLocator;
import org.sonatype.nexus.proxy.item.DefaultStorageFileItem;
import org.sonatype.nexus.proxy.registry.ContentClass;
import org.sonatype.nexus.proxy.repository.AbstractProxyRepository;
import org.sonatype.nexus.proxy.repository.DefaultRepositoryKind;
import org.sonatype.nexus.proxy.repository.RepositoryKind;
import org.sonatype.nexus.proxy.walker.WalkerFilter;

import javax.inject.Inject;
import javax.inject.Named;

import static com.google.common.base.Preconditions.checkNotNull;
import static org.sonatype.nexus.proxy.ItemNotFoundException.reasonFor;

/**
 * @author Georgy Bolyuba (georgy@bolyuba.com)
 */
@Named(DefaultNpmProxyRepository.ROLE_HINT)
@Description("Npm registry proxy repo")
public class DefaultNpmProxyRepository
        extends AbstractProxyRepository
        implements NpmProxyRepository, NpmRepository {

    public static final String ROLE_HINT = "npm-proxy";

    private final ContentClass contentClass;

    private final NpmProxyRepositoryConfigurator configurator;

    private final RepositoryKind repositoryKind;

    private final NpmMimeRulesSource mimeRulesSource;

    private final ProxyMetadataService proxyMetadataService;

    @Inject
    public DefaultNpmProxyRepository(final @Named(NpmContentClass.ID) ContentClass contentClass,
                                     final NpmProxyRepositoryConfigurator configurator,
                                     final MetadataServiceFactory metadataServiceFactory) {

        this.proxyMetadataService = metadataServiceFactory.createProxyMetadataService(this);
        this.contentClass = checkNotNull(contentClass);
        this.configurator = checkNotNull(configurator);

        this.repositoryKind = new DefaultRepositoryKind(NpmProxyRepository.class, null);
        this.mimeRulesSource = new NpmMimeRulesSource();
    }

    @Override
    protected Configurator getConfigurator() {
        return configurator;
    }

    @Override
    public RepositoryKind getRepositoryKind() {
        return repositoryKind;
    }

    @Override
    public ContentClass getRepositoryContentClass() {
        return contentClass;
    }

    @Override
    protected CRepositoryExternalConfigurationHolderFactory<NpmProxyRepositoryConfiguration> getExternalConfigurationHolderFactory() {
        return new CRepositoryExternalConfigurationHolderFactory<NpmProxyRepositoryConfiguration>() {
            @Override
            public NpmProxyRepositoryConfiguration createExternalConfigurationHolder(final CRepository config) {
                return new NpmProxyRepositoryConfiguration((Xpp3Dom) config.getExternalConfiguration());
            }
        };
    }

    @Override
    public MimeRulesSource getMimeRulesSource() {
        return mimeRulesSource;
    }

    @Override
    protected boolean doExpireProxyCaches(final ResourceStoreRequest request, final WalkerFilter filter) {
        boolean result = super.doExpireProxyCaches(request, filter);
        try {
          boolean npmResult = proxyMetadataService.expireMetadataCaches(new PackageRequest(request));
          return result || npmResult;
        } catch (IllegalArgumentException ignore) {
          // ignore
          return result;
        }
    }

    @Override
    protected AbstractStorageItem doRetrieveLocalItem(ResourceStoreRequest storeRequest) throws ItemNotFoundException, LocalStorageException {
        try {
            PackageRequest packageRequest = new PackageRequest(storeRequest);
            packageRequest.getStoreRequest().getRequestContext().put(NpmRepository.NPM_METADATA_SERVICED, Boolean.TRUE);
            if (packageRequest.isMetadata()) {
              ContentLocator contentLocator;
              if (packageRequest.isRegistryRoot()) {
                contentLocator = proxyMetadataService.produceRegistryRoot(packageRequest);
              }
              else if (packageRequest.isPackageRoot()) {
                contentLocator = proxyMetadataService.producePackageRoot(packageRequest);
              }
              else {
                contentLocator = proxyMetadataService.producePackageVersion(packageRequest);
              }
              if (contentLocator == null) {
                log.debug("No NPM metadata for path {}", storeRequest.getRequestPath());
                throw new ItemNotFoundException(
                    reasonFor(storeRequest, this, "No content for path %s", storeRequest.getRequestPath()));
              }
              return new DefaultStorageFileItem(this, storeRequest, true, true, contentLocator);
            }
            else {
              // registry special
              if (packageRequest.isRegistrySpecial() && packageRequest.getPath().startsWith("/-/all")) {
                return new DefaultStorageFileItem(this, storeRequest, true, true,
                    proxyMetadataService.produceRegistryRoot(packageRequest));
              }
              throw new ItemNotFoundException(
                  reasonFor(storeRequest, this, "No content for path %s", storeRequest.getRequestPath()));
            }
        } catch (IllegalArgumentException ignore) {
            return delegateDoRetrieveLocalItem(storeRequest);
        } catch (IOException e) {
          throw new LocalStorageException("Metadata service error", e);
        }
    }

    /**
     * Beside original behaviour, only try remote the non-metadata requests.
     */
    @Override
    protected void shouldTryRemote(final ResourceStoreRequest request)
        throws IllegalOperationException, ItemNotFoundException
    {
      super.shouldTryRemote(request);
      if (request.getRequestContext().containsKey(NpmRepository.NPM_METADATA_SERVICED)) {
        throw new ItemNotFoundException(ItemNotFoundException.reasonFor(request, this,
            "Request is serviced by NPM metadata service, remote access not needed from %s", this));
      }
    }

    /**
     * Beside original behavior, only add to NFC non-metadata requests.
     */
    @Override
    protected boolean shouldAddToNotFoundCache(final ResourceStoreRequest request) {
      boolean shouldAddToNFC = super.shouldAddToNotFoundCache(request);
      if (shouldAddToNFC) {
        return !request.getRequestContext().containsKey(NpmRepository.NPM_METADATA_SERVICED);
      }
      return shouldAddToNFC;
    }

    @Override
    public AbstractStorageItem doCacheItem(AbstractStorageItem item) throws LocalStorageException {
        try {
            ResourceStoreRequest storeRequest = item.getResourceStoreRequest();
            PackageRequest packageRequest = new PackageRequest(storeRequest);
            log.info("NPM cache {}", packageRequest.getPath());
            if (packageRequest.isMetadata()) {
              // no cache, is done by MetadataService (should not get here at all)
              return item;
            } else {
              return delegateDoCacheItem(item);
            }
        } catch (IllegalArgumentException ignore) {
            // do it old style
            return delegateDoCacheItem(item);
        }
    }

    AbstractStorageItem delegateDoCacheItem(AbstractStorageItem item) throws LocalStorageException {
        return super.doCacheItem(item);
    }

    AbstractStorageItem delegateDoRetrieveLocalItem(ResourceStoreRequest storeRequest) throws LocalStorageException, ItemNotFoundException {
        return super.doRetrieveLocalItem(storeRequest);
    }
}
package com.bolyuba.nexus.plugin.npm.hosted;

import com.bolyuba.nexus.plugin.npm.NpmContentClass;
import com.bolyuba.nexus.plugin.npm.NpmRepository;
import com.bolyuba.nexus.plugin.npm.content.NpmMimeRulesSource;
import com.bolyuba.nexus.plugin.npm.metadata.HostedMetadataService;
import com.bolyuba.nexus.plugin.npm.metadata.MetadataServiceFactory;
import com.bolyuba.nexus.plugin.npm.metadata.PackageAttachment;
import com.bolyuba.nexus.plugin.npm.metadata.PackageRoot;
import com.bolyuba.nexus.plugin.npm.metadata.Producer;
import com.bolyuba.nexus.plugin.npm.pkg.PackageRequest;
import org.codehaus.plexus.util.xml.Xpp3Dom;
import org.sonatype.inject.Description;
import org.sonatype.nexus.configuration.Configurator;
import org.sonatype.nexus.configuration.model.CRepository;
import org.sonatype.nexus.configuration.model.CRepositoryExternalConfigurationHolderFactory;
import org.sonatype.nexus.mime.MimeRulesSource;
import org.sonatype.nexus.proxy.AccessDeniedException;
import org.sonatype.nexus.proxy.IllegalOperationException;
import org.sonatype.nexus.proxy.ItemNotFoundException;
import org.sonatype.nexus.proxy.LocalStorageException;
import org.sonatype.nexus.proxy.ResourceStoreRequest;
import org.sonatype.nexus.proxy.StorageException;
import org.sonatype.nexus.proxy.access.Action;
import org.sonatype.nexus.proxy.item.AbstractStorageItem;
import org.sonatype.nexus.proxy.item.ContentLocator;
import org.sonatype.nexus.proxy.item.DefaultStorageFileItem;
import org.sonatype.nexus.proxy.item.PreparedContentLocator;
import org.sonatype.nexus.proxy.item.RepositoryItemUid;
import org.sonatype.nexus.proxy.item.RepositoryItemUidLock;
import org.sonatype.nexus.proxy.registry.ContentClass;
import org.sonatype.nexus.proxy.repository.AbstractRepository;
import org.sonatype.nexus.proxy.repository.DefaultRepositoryKind;
import org.sonatype.nexus.proxy.repository.RepositoryKind;
import org.sonatype.nexus.proxy.storage.UnsupportedStorageOperationException;

import javax.inject.Inject;
import javax.inject.Named;
import java.io.IOException;
import java.io.InputStream;
import java.util.Map;

import static com.google.common.base.Preconditions.checkNotNull;
import static org.sonatype.nexus.proxy.ItemNotFoundException.reasonFor;

/**
 * @author Georgy Bolyuba (georgy@bolyuba.com)
 */
@Named(DefaultNpmHostedRepository.ROLE_HINT)
@Description("Npm registry hosted repo")
public class DefaultNpmHostedRepository
        extends AbstractRepository
        implements NpmHostedRepository, NpmRepository {

    public static final String ROLE_HINT = "npm-hosted";

    private final ContentClass contentClass;

    private final NpmHostedRepositoryConfigurator configurator;

    private final RepositoryKind repositoryKind;

    private final NpmMimeRulesSource mimeRulesSource;

    private final HostedMetadataService hostedMetadataService;

    @Inject
    public DefaultNpmHostedRepository(final @Named(NpmContentClass.ID) ContentClass contentClass,
                                      final NpmHostedRepositoryConfigurator configurator,
                                      final MetadataServiceFactory metadataServiceFactory) {

        this.hostedMetadataService = metadataServiceFactory.createHostedMetadataService(this);
        this.mimeRulesSource = new NpmMimeRulesSource();
        this.contentClass = checkNotNull(contentClass);
        this.configurator = checkNotNull(configurator);
        this.repositoryKind = new DefaultRepositoryKind(NpmHostedRepository.class, null);
    }

    @Override
    protected Configurator getConfigurator() {
        return this.configurator;
    }

    @Override
    public RepositoryKind getRepositoryKind() {
        return this.repositoryKind;
    }

    @Override
    public ContentClass getRepositoryContentClass() {
        return this.contentClass;
    }

    @Override
    public MimeRulesSource getMimeRulesSource() {
        return mimeRulesSource;
    }

    @Override
    protected CRepositoryExternalConfigurationHolderFactory<?> getExternalConfigurationHolderFactory() {
        return new CRepositoryExternalConfigurationHolderFactory<NpmHostedRepositoryConfiguration>() {
            @Override
            public NpmHostedRepositoryConfiguration createExternalConfigurationHolder(final CRepository config) {
                return new NpmHostedRepositoryConfiguration((Xpp3Dom) config.getExternalConfiguration());
            }
        };
    }

    @Override
    protected AbstractStorageItem doRetrieveLocalItem(ResourceStoreRequest storeRequest) throws ItemNotFoundException, LocalStorageException {
        try {
            PackageRequest packageRequest = new PackageRequest(storeRequest);
            if (packageRequest.isMetadata()) {
              ContentLocator contentLocator;
              if (packageRequest.isRegistryRoot()) {
                contentLocator = hostedMetadataService.produceRegistryRoot(packageRequest);
              } else if (packageRequest.isPackageRoot()) {
                contentLocator = hostedMetadataService.producePackageRoot(packageRequest);
              } else {
                contentLocator = hostedMetadataService.producePackageVersion(packageRequest);
              }
              if (contentLocator == null) {
                throw new ItemNotFoundException(reasonFor(storeRequest, this, "No content for path %s", storeRequest.getRequestPath()));
              }
              return new DefaultStorageFileItem(this, storeRequest, true, true, contentLocator);
            } else {
                // registry special
                if (packageRequest.isRegistrySpecial() && packageRequest.getPath().startsWith("/-/all")) {
                  return new DefaultStorageFileItem(this, storeRequest, true, true, hostedMetadataService.produceRegistryRoot(packageRequest));
                }
                throw new ItemNotFoundException(reasonFor(storeRequest, this, "No content for path %s", storeRequest.getRequestPath()));
            }
        } catch (IllegalArgumentException ignore) {
            // something completely different
            return delegateDoRetrieveLocalItem(storeRequest);
        } catch (IOException e) {
          throw new LocalStorageException("Metadata service error", e);
        }
    }

    AbstractStorageItem delegateDoRetrieveLocalItem(ResourceStoreRequest storeRequest) throws LocalStorageException, ItemNotFoundException {
        return super.doRetrieveLocalItem(storeRequest);
    }

    @SuppressWarnings("deprecation")
    @Override
    public void storeItem(ResourceStoreRequest request, InputStream is, Map<String, String> userAttributes)
            throws UnsupportedStorageOperationException, IllegalOperationException, StorageException, AccessDeniedException {
        try {
            PackageRequest packageRequest = new PackageRequest(request);

            if (!packageRequest.isPackageRoot()) {
                throw new UnsupportedStorageOperationException("Store operations are only valid for package roots, path: " + packageRequest.getPath());
            }

            // serialize all publish request for the same
            final RepositoryItemUid publisherUid = createUid(packageRequest.getPath() + ".publish()");
            RepositoryItemUidLock publisherLock = publisherUid.getLock();

            publisherLock.lock(Action.create);
            try {
                final PackageRoot packageRoot = hostedMetadataService.consumePackageRoot(packageRequest, new PreparedContentLocator(is, NpmRepository.JSON_MIME_TYPE, ContentLocator.UNKNOWN_LENGTH));

                if (!packageRoot.getAttachments().isEmpty()) {
                  for (PackageAttachment attachment : packageRoot.getAttachments().values()) {
                    final ResourceStoreRequest attachmentRequest = new ResourceStoreRequest(request);
                    attachmentRequest.setRequestPath(packageRequest.getPath() + RepositoryItemUid.PATH_SEPARATOR + NPM_REGISTRY_SPECIAL +
                        RepositoryItemUid.PATH_SEPARATOR + attachment.getName());
                    super.storeItem(attachmentRequest, attachment.getContent(), userAttributes);
                  }
                }
            } finally {
                publisherLock.unlock();
            }
        } catch (IllegalArgumentException e) {
            // TODO: This might be our tarball, but it also might be something stupid uploaded. Need to validate further
            // for now just store it
            super.storeItem(request, is, userAttributes);
        } catch (IOException e) {
          throw new LocalStorageException("Upload problem", e);
        }
    }
}
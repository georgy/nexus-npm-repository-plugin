package com.bolyuba.nexus.plugin.npm.group;

import java.io.IOException;

import javax.enterprise.inject.Typed;
import javax.inject.Inject;
import javax.inject.Named;

import org.sonatype.nexus.configuration.Configurator;
import org.sonatype.nexus.configuration.model.CRepository;
import org.sonatype.nexus.configuration.model.CRepositoryExternalConfigurationHolderFactory;
import org.sonatype.nexus.mime.MimeRulesSource;
import org.sonatype.nexus.proxy.ItemNotFoundException;
import org.sonatype.nexus.proxy.LocalStorageException;
import org.sonatype.nexus.proxy.ResourceStoreRequest;
import org.sonatype.nexus.proxy.item.AbstractStorageItem;
import org.sonatype.nexus.proxy.item.ContentLocator;
import org.sonatype.nexus.proxy.item.DefaultStorageFileItem;
import org.sonatype.nexus.proxy.registry.ContentClass;
import org.sonatype.nexus.proxy.repository.AbstractGroupRepository;
import org.sonatype.nexus.proxy.repository.DefaultRepositoryKind;
import org.sonatype.nexus.proxy.repository.GroupRepository;
import org.sonatype.nexus.proxy.repository.RepositoryKind;

import com.bolyuba.nexus.plugin.npm.NpmContentClass;
import com.bolyuba.nexus.plugin.npm.internal.NpmMimeRulesSource;
import com.bolyuba.nexus.plugin.npm.service.GroupMetadataService;
import com.bolyuba.nexus.plugin.npm.service.MetadataServiceFactory;
import com.bolyuba.nexus.plugin.npm.service.PackageRequest;
import org.codehaus.plexus.util.xml.Xpp3Dom;
import org.eclipse.sisu.Description;

import static com.google.common.base.Preconditions.checkNotNull;
import static org.sonatype.nexus.proxy.ItemNotFoundException.reasonFor;

/**
 * @author Georgy Bolyuba (georgy@bolyuba.com)
 */
@Named(DefaultNpmGroupRepository.ROLE_HINT)
@Typed(GroupRepository.class)
@Description("Npm registry group repo")
public class DefaultNpmGroupRepository
        extends AbstractGroupRepository
        implements NpmGroupRepository, GroupRepository  {

    public static final String ROLE_HINT = "npm-group";

    private final ContentClass contentClass;

    private final NpmGroupRepositoryConfigurator configurator;

    private final RepositoryKind repositoryKind;

    private final NpmMimeRulesSource mimeRulesSource;

    private final GroupMetadataService groupMetadataService;

    @Inject
    public DefaultNpmGroupRepository(final @Named(NpmContentClass.ID) ContentClass contentClass,
                                     final NpmGroupRepositoryConfigurator configurator,
                                     final MetadataServiceFactory metadataServiceFactory) {

        this.groupMetadataService = metadataServiceFactory.createGroupMetadataService(this);
        this.mimeRulesSource = new NpmMimeRulesSource();
        this.contentClass = checkNotNull(contentClass);
        this.configurator = checkNotNull(configurator);
        this.repositoryKind = new DefaultRepositoryKind(NpmGroupRepository.class, null);
    }

    @Override
    public GroupMetadataService getMetadataService() { return groupMetadataService; }

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
        return new CRepositoryExternalConfigurationHolderFactory<NpmGroupRepositoryConfiguration>() {
            @Override
            public NpmGroupRepositoryConfiguration createExternalConfigurationHolder(final CRepository config) {
                return new NpmGroupRepositoryConfiguration((Xpp3Dom) config.getExternalConfiguration());
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
                contentLocator = groupMetadataService.getProducer().produceRegistryRoot(packageRequest);
              } else if (packageRequest.isPackageRoot()) {
                contentLocator = groupMetadataService.getProducer().producePackageRoot(packageRequest);
              } else {
                contentLocator = groupMetadataService.getProducer().producePackageVersion(packageRequest);
              }
              if (contentLocator == null) {
                throw new ItemNotFoundException(reasonFor(storeRequest, this, "No content for path %s", storeRequest.getRequestPath()));
              }
              return new DefaultStorageFileItem(this, storeRequest, true, true, contentLocator);
            } else {
                // registry special
                if (packageRequest.isRegistrySpecial() && packageRequest.getPath().startsWith("/-/all")) {
                  return new DefaultStorageFileItem(this, storeRequest, true, true, groupMetadataService.getProducer().produceRegistryRoot(packageRequest));
                }
                throw new ItemNotFoundException(reasonFor(storeRequest, this, "No content for path %s", storeRequest.getRequestPath()));
            }
        } catch (IllegalArgumentException ignore) {
            // something completely different
            return super.doRetrieveLocalItem(storeRequest);
        } catch (IOException e) {
          throw new LocalStorageException("Metadata service error", e);
        }
    }
}
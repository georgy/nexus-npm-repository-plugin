package com.bolyuba.nexus.plugin.npm.metadata.internal;

import java.io.IOException;
import java.util.Iterator;
import java.util.List;

import javax.annotation.Nullable;

import org.sonatype.nexus.proxy.repository.Repository;

import com.bolyuba.nexus.plugin.npm.NpmRepository;
import com.bolyuba.nexus.plugin.npm.group.NpmGroupRepository;
import com.bolyuba.nexus.plugin.npm.metadata.GroupMetadataService;
import com.bolyuba.nexus.plugin.npm.metadata.PackageRoot;
import com.bolyuba.nexus.plugin.npm.metadata.PackageVersion;
import com.bolyuba.nexus.plugin.npm.pkg.PackageRequest;
import com.google.common.collect.Iterators;
import com.google.common.collect.Lists;

/**
 * {@link GroupMetadataService} implementation.
 */
public class GroupMetadataServiceImpl
    extends GeneratorSupport
    implements GroupMetadataService
{
  private final NpmGroupRepository npmGroupRepository;

  public GroupMetadataServiceImpl(final NpmGroupRepository npmGroupRepository,
                                  final MetadataParser metadataParser)
  {
    super(metadataParser);
    this.npmGroupRepository = npmGroupRepository;
  }

  @Override
  protected PackageRootIterator doGenerateRegistryRoot(final PackageRequest request) throws IOException {
    final List<NpmRepository> members = getMembers();
    final List<PackageRootIterator> iterators = Lists.newArrayList();
    for (NpmRepository member : members) {
      // TODO: duplicate packages???
      iterators.add(member.getMetadataService().generateRegistryRoot(request));
    }
    return new AggregatedPackageRootIterator(iterators);
  }

  @Nullable
  @Override
  protected PackageRoot doGeneratePackageRoot(final PackageRequest request) throws IOException {
    final List<NpmRepository> members = getMembers();
    for (NpmRepository member : members) {
      final PackageRoot root = member.getMetadataService().generatePackageRoot(request);
      if (root != null) {
        return root;
      }
    }
    return null;
  }

  @Nullable
  @Override
  protected PackageVersion doGeneratePackageVersion(final PackageRequest request) throws IOException {
    final List<NpmRepository> members = getMembers();
    for (NpmRepository member : members) {
      final PackageVersion version = member.getMetadataService().generatePackageVersion(request);
      if (version != null) {
        return version;
      }
    }
    return null;
  }

  // ==

  /**
   * Returns group's members that are for certain NPM repositories.
   */
  private List<NpmRepository> getMembers() {
    final List<Repository> members = npmGroupRepository.getMemberRepositories();
    final List<NpmRepository> npmMembers = Lists.newArrayList();
    for (Repository member : members) {
      final NpmRepository npmMember = member.adaptToFacet(NpmRepository.class);
      if (npmMember != null) {
        npmMembers.add(npmMember);
      }
    }
    return npmMembers;
  }

  // ==

  private static class AggregatedPackageRootIterator
      implements PackageRootIterator
  {
    private final List<PackageRootIterator> iterators;

    private final Iterator<PackageRoot> iterator;

    private AggregatedPackageRootIterator(final List<PackageRootIterator> iterators) {
      this.iterators = iterators;
      this.iterator = Iterators.concat(iterators.iterator());
    }

    @Override
    public void close() {
      for (PackageRootIterator iterator : iterators) {
        try {
          iterator.close();
        }
        catch (IOException e) {
          // swallow
        }
      }
    }

    @Override
    public boolean hasNext() {
      boolean hasNext = iterator.hasNext();
      if (!hasNext) {
        close();
      }
      return hasNext;
    }

    @Override
    public PackageRoot next() {
      return iterator.next();
    }

    @Override
    public void remove() {
      throw new UnsupportedOperationException("Remove not supported");
    }
  }
}

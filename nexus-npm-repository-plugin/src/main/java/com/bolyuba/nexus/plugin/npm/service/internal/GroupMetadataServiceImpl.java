package com.bolyuba.nexus.plugin.npm.service.internal;

import java.io.IOException;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.NoSuchElementException;

import javax.annotation.Nullable;

import org.sonatype.nexus.proxy.repository.Repository;

import com.bolyuba.nexus.plugin.npm.NpmRepository;
import com.bolyuba.nexus.plugin.npm.group.NpmGroupRepository;
import com.bolyuba.nexus.plugin.npm.service.GroupMetadataService;
import com.bolyuba.nexus.plugin.npm.service.PackageRequest;
import com.bolyuba.nexus.plugin.npm.service.PackageRoot;
import com.bolyuba.nexus.plugin.npm.service.PackageVersion;
import com.google.common.collect.Lists;
import com.google.common.collect.Sets;

import static com.google.common.base.Preconditions.checkNotNull;

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
    super(npmGroupRepository, metadataParser);
    this.npmGroupRepository = checkNotNull(npmGroupRepository);
  }

  @Override
  protected PackageRootIterator doGenerateRegistryRoot(final PackageRequest request) throws IOException {
    final List<NpmRepository> members = getMembers();
    final List<PackageRootIterator> iterators = Lists.newArrayList();
    for (NpmRepository member : members) {
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

  /**
   * Aggregates multiple {@link PackageRootIterator}s but keeps package names unique.
   */
  private static class AggregatedPackageRootIterator
      implements PackageRootIterator
  {
    private final List<PackageRootIterator> iterators;

    private final Iterator<PackageRootIterator> iteratorsIterator;

    private final HashSet<String> names;

    private PackageRootIterator currentIterator;

    private PackageRoot next;

    private AggregatedPackageRootIterator(final List<PackageRootIterator> iterators) {
      this.iterators = iterators;
      this.iteratorsIterator = iterators.iterator();
      this.names = Sets.newHashSet();
      this.currentIterator = iteratorsIterator.hasNext() ? iteratorsIterator.next() : PackageRootIterator.EMPTY;
      this.next = getNext();
    }

    private PackageRoot getNext() {
      while (currentIterator != PackageRootIterator.EMPTY) {
        while (currentIterator.hasNext()) {
          final PackageRoot next = currentIterator.next();
          if (names.add(next.getName())) {
            return next;
          }
        }
        if (iteratorsIterator.hasNext()) {
          currentIterator = iteratorsIterator.next();
        }
        else {
          currentIterator = PackageRootIterator.EMPTY;
        }
      }
      return null; // no more
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
      boolean hasNext = next != null;
      if (!hasNext) {
        close();
      }
      return hasNext;
    }

    @Override
    public PackageRoot next() {
      final PackageRoot result = next;
      if (result == null) {
        throw new NoSuchElementException("Iterator depleted");
      }
      next = getNext();
      if (next == null) {
        close();
      }
      return result;
    }

    @Override
    public void remove() {
      throw new UnsupportedOperationException("Remove not supported");
    }
  }
}

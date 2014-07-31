package com.bolyuba.nexus.plugin.npm.metadata.internal;

import java.io.IOException;
import java.io.InputStream;
import java.util.Iterator;
import java.util.List;

import org.sonatype.nexus.proxy.item.AbstractContentLocator;
import org.sonatype.nexus.proxy.item.ContentLocator;

import com.bolyuba.nexus.plugin.npm.NpmRepository;
import com.google.common.base.Charsets;
import com.google.common.base.Throwables;
import com.google.common.collect.Lists;
import com.google.common.io.ByteSource;

import static com.google.common.base.Preconditions.checkNotNull;

/**
 * A content locator that streams the potentially huge registry root JSON document out of all of the package documents.
 */
public class RegistryRootContentLocator
    extends AbstractContentLocator
    implements Iterator<ByteSource>
{
  private final MetadataProducerImpl metadataProducer;

  private final Iterator<String> packageNameIterator;

  private boolean first;

  protected RegistryRootContentLocator(final MetadataProducerImpl metadataProducer, final List<String> packageNames) {
    super(NpmRepository.JSON_MIME_TYPE, false, ContentLocator.UNKNOWN_LENGTH);
    this.metadataProducer = checkNotNull(metadataProducer);
    this.packageNameIterator = checkNotNull(packageNames).iterator();
    this.first = true;
  }

  @Override
  public InputStream getContent() throws IOException {
    return ByteSource.concat(this).openStream();
  }

  // ==

  @Override
  public boolean hasNext() {
    return first || packageNameIterator.hasNext();
  }

  @Override
  public ByteSource next() {
    try {
      final List<ByteSource> sources = Lists.newArrayList();
      if (first) {
        first = false;
        sources.add(ByteSource.wrap("{".getBytes(Charsets.UTF_8)));
      }
      if (packageNameIterator.hasNext()) {
        final String packageName = packageNameIterator.next();
        sources.add(ByteSource.wrap(("\"" + packageName + "\":").getBytes(Charsets.UTF_8)));
        sources.add(ByteSource.wrap(metadataProducer.produceShrinkedPackageRoot(packageName).getByteArray()));
      }
      if (!packageNameIterator.hasNext()) {
        sources.add(ByteSource.wrap("}".getBytes(Charsets.UTF_8)));
      }
      else {
        sources.add(ByteSource.wrap(",".getBytes(Charsets.UTF_8)));
      }
      return ByteSource.concat(sources);
    }
    catch (Exception e) {
      throw Throwables.propagate(e);
    }
  }

  @Override
  public void remove() {
    throw new UnsupportedOperationException("Remove not supported");
  }
}

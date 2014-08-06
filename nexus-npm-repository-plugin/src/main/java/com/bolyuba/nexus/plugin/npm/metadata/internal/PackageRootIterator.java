package com.bolyuba.nexus.plugin.npm.metadata.internal;

import java.io.Closeable;
import java.io.IOException;
import java.util.Iterator;
import java.util.NoSuchElementException;

import com.bolyuba.nexus.plugin.npm.metadata.PackageRoot;

/**
 * {@link Iterator} of {@link PackageRoot}s. Should be handled as resources, hence is {@link Closeable}. Preferred use
 * is within try-with-resource block.
 */
public interface PackageRootIterator
    extends Iterator<PackageRoot>, Closeable
{
  PackageRootIterator EMPTY = new PackageRootIterator()
  {
    @Override
    public void close() throws IOException {
      // nop
    }

    @Override
    public boolean hasNext() {
      return false;
    }

    @Override
    public PackageRoot next() {
      throw new NoSuchElementException("Empty iterator");
    }

    @Override
    public void remove() {
      throw new UnsupportedOperationException("Remove not supported!");
    }
  };
}

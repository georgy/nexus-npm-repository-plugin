package com.bolyuba.nexus.plugin.npm.metadata.internal;

import java.io.Closeable;
import java.util.Iterator;

import com.bolyuba.nexus.plugin.npm.metadata.PackageRoot;

/**
 * {@link Iterator} of {@link PackageRoot}s. Should be handled as resources, hence is {@link Closeable}. Preferred use
 * is within try-with-resource block.
 */
public interface PackageRootIterator
    extends Iterator<PackageRoot>, Closeable
{
}

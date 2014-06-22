package com.bolyuba.nexus.plugin.npm.hosted;

import org.sonatype.nexus.proxy.IllegalOperationException;

/**
 * Thrown when operation is not understood by hosted repository
 *
 * @author Georgy Bolyuba (georgy@bolyuba.com)
 */
public class InvalidRegistryOperationException extends IllegalOperationException {

    public InvalidRegistryOperationException(String message) {
        super(message);
    }
}

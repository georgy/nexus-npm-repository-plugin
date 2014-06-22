package com.bolyuba.nexus.plugin.npm.pkg;

/**
 * @author <a href="mailto:georgy@bolyuba.com">Georgy Bolyuba</a>
 */
public class InvalidPackageRequestException extends Exception {

    public InvalidPackageRequestException(String message) {
        super(message);
    }
}

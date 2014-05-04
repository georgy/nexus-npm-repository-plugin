package com.bolyuba.nexus.plugin.npm.content;

import com.bolyuba.nexus.plugin.npm.NpmRepository;
import org.sonatype.nexus.proxy.item.PreparedContentLocator;

import javax.annotation.Nonnull;
import java.io.ByteArrayInputStream;

/**
 * Content locator for in-memory json. Think before you use it
 *
 * @author Georgy Bolyuba (georgy@bolyuba.com)
 */
public class NpmJsonContentLocator
        extends PreparedContentLocator {

    public NpmJsonContentLocator(@Nonnull String json) {
        super(new ByteArrayInputStream(json.getBytes()), NpmRepository.JSON_MIME_TYPE, json.getBytes().length);
    }
}

package com.bolyuba.nexus.plugin.npm.content;

import com.bolyuba.nexus.plugin.npm.NpmRepository;
import com.google.common.base.Charsets;
import org.sonatype.nexus.proxy.item.ByteArrayContentLocator;

import javax.annotation.Nonnull;

/**
 * Content locator for in-memory json. Think before you use it
 *
 * @author Georgy Bolyuba (georgy@bolyuba.com)
 */
public class NpmJsonContentLocator
        extends ByteArrayContentLocator
{

    public NpmJsonContentLocator(@Nonnull String json) {
        super(json.getBytes(Charsets.UTF_8), NpmRepository.JSON_MIME_TYPE);
    }
}

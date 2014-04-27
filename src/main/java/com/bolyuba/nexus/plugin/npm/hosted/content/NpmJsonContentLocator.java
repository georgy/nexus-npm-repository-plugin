package com.bolyuba.nexus.plugin.npm.hosted.content;

import org.sonatype.nexus.proxy.item.ContentLocator;

import javax.annotation.Nonnull;
import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;

/**
 * @author Georgy Bolyuba (georgy@bolyuba.com)
 */
public class NpmJsonContentLocator
        implements ContentLocator {

    private final ByteArrayInputStream inputStream;

    public NpmJsonContentLocator(@Nonnull String json) {
        this.inputStream = new ByteArrayInputStream(json.getBytes());
    }

    @Override
    public InputStream getContent() throws IOException {
        return inputStream;
    }

    @Override
    public String getMimeType() {
        return "application/json";
    }

    @Override
    public long getLength() {
        return ContentLocator.UNKNOWN_LENGTH;
    }

    @Override
    public boolean isReusable() {
        return false;
    }
}

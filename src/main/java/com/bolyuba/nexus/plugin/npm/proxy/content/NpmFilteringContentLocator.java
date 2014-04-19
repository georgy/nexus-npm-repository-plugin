package com.bolyuba.nexus.plugin.npm.proxy.content;

import org.sonatype.nexus.proxy.ResourceStoreRequest;
import org.sonatype.nexus.proxy.item.AbstractWrappingContentLocator;
import org.sonatype.nexus.proxy.item.ContentLocator;

import java.io.IOException;
import java.io.InputStream;

/**
 * @author Georgy Bolyuba (georgy@bolyuba.com)
 */
public class NpmFilteringContentLocator extends AbstractWrappingContentLocator {

    private final Mapping mapping;

    public NpmFilteringContentLocator(ContentLocator contentLocator, ResourceStoreRequest request, String remoteUrl) {
        super(contentLocator);
        mapping = new Mapping(remoteUrl, getToUrl(request));
    }

    @Override
    public InputStream getContent() throws IOException {
        return new NpmFilterInputStream(getTarget().getContent(), mapping);
    }

    private String getToUrl(ResourceStoreRequest request) {
        String url = request.getRequestUrl();
        return url.substring(0, url.length() - request.getRequestPath().length());
    }
}

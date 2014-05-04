package com.bolyuba.nexus.plugin.npm.content;

import com.bolyuba.nexus.plugin.npm.content.filtering.JsonFilter;
import com.bolyuba.nexus.plugin.npm.content.filtering.JsonFilterInputStream;
import com.bolyuba.nexus.plugin.npm.content.filtering.JsonMappingFilter;
import com.bolyuba.nexus.plugin.npm.content.filtering.Mapping;
import org.sonatype.nexus.proxy.ResourceStoreRequest;
import org.sonatype.nexus.proxy.item.AbstractWrappingContentLocator;
import org.sonatype.nexus.proxy.item.ContentLocator;

import java.io.IOException;
import java.io.InputStream;

/**
 * @author Georgy Bolyuba (georgy@bolyuba.com)
 */
public class TarballUrlFilteringContentLocator extends AbstractWrappingContentLocator {

    private JsonFilter tarballFilter;

    public TarballUrlFilteringContentLocator(ContentLocator contentLocator, ResourceStoreRequest request, String remoteUrl) {
        super(contentLocator);
        tarballFilter = new JsonMappingFilter("tarball", new Mapping(remoteUrl, getToUrl(request)));
    }

    @Override
    public InputStream getContent() throws IOException {
        return new JsonFilterInputStream(getTarget().getContent(), tarballFilter);
    }

    private String getToUrl(ResourceStoreRequest request) {
        String url = request.getRequestUrl();
        return url.substring(0, url.length() - request.getRequestPath().length());
    }
}

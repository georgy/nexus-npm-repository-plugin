package com.bolyuba.nexus.plugin.npm.proxy.content;

import org.sonatype.nexus.proxy.item.AbstractWrappingContentLocator;
import org.sonatype.nexus.proxy.item.ContentLocator;

import java.io.IOException;
import java.io.InputStream;

/**
 * @author Georgy Bolyuba (georgy@bolyuba.com)
 */
public class NpmFilteringContentLocator extends AbstractWrappingContentLocator {

    public NpmFilteringContentLocator(ContentLocator contentLocator) {
        super(contentLocator);
    }

    @Override
    public InputStream getContent() throws IOException {
        return new NpmFilterInputStream(getTarget().getContent());
    }


}

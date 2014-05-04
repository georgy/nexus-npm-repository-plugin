package com.bolyuba.nexus.plugin.npm.content;

import com.bolyuba.nexus.plugin.npm.NpmRepository;
import com.bolyuba.nexus.plugin.npm.content.filtering.JsonFilterInputStream;
import com.bolyuba.nexus.plugin.npm.content.filtering.PackageRootMergingFilter;
import com.bolyuba.nexus.plugin.npm.content.filtering.PackageRootOverwritingFilter;
import org.sonatype.nexus.proxy.item.AbstractWrappingContentLocator;
import org.sonatype.nexus.proxy.item.ContentLocator;

import java.io.IOException;
import java.io.InputStream;

/**
 * @author Georgy Bolyuba (georgy@bolyuba.com)
 */
public class PackageRootContentLocator extends AbstractWrappingContentLocator {

    private ContentLocator existing;

    public PackageRootContentLocator(ContentLocator mixing) {
        super(mixing);
    }

    public PackageRootContentLocator(ContentLocator mixing, ContentLocator existing) {
        this(mixing);
        this.existing = existing;
    }

    @Override
    public InputStream getContent() throws IOException {
        if (existing == null) {
            return new JsonFilterInputStream(getTarget().getContent(), new PackageRootOverwritingFilter());
        } else {
            return new JsonFilterInputStream(getTarget().getContent(), new PackageRootMergingFilter());
        }
    }

    @Override
    public String getMimeType() {
        return NpmRepository.JSON_MIME_TYPE;
    }
}

package com.bolyuba.nexus.plugin.npm.proxy.content;

import com.bolyuba.nexus.plugin.npm.NpmRepository;
import org.sonatype.nexus.mime.MimeRulesSource;

/**
 * @author Georgy Bolyuba (georgy@bolyuba.com)
 */
public class NpmMimeRulesSource
        implements MimeRulesSource {

    @Override
    public String getRuleForPath(String path) {
        if (path == null) {
            return null;
        }

        if (path.toLowerCase().endsWith(NpmRepository.JSON_CONTENT_FILE_NAME)) {
            return NpmRepository.JSON_MIME_TYPE;
        }
        return null;
    }
}

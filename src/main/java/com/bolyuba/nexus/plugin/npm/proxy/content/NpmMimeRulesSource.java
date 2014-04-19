package com.bolyuba.nexus.plugin.npm.proxy.content;

import com.bolyuba.nexus.plugin.npm.proxy.NpmUtility;
import org.sonatype.nexus.mime.MimeRulesSource;

import javax.inject.Inject;
import javax.inject.Named;
import javax.inject.Singleton;

/**
 * @author Georgy Bolyuba (georgy@bolyuba.com)
 */
@Named
@Singleton
public class NpmMimeRulesSource
        implements MimeRulesSource {

    private final NpmUtility utility;

    @Inject
    public NpmMimeRulesSource(NpmUtility utility) {
        this.utility = utility;
    }

    @Override
    public String getRuleForPath(String path) {
        if (path == null) {
            return null;
        }
        return utility.suggetMimeType(path);
    }
}

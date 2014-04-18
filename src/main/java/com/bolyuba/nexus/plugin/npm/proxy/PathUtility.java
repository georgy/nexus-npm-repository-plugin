package com.bolyuba.nexus.plugin.npm.proxy;

import javax.annotation.Nonnull;

/**
 * @author Georgy Bolyuba (georgy@bolyuba.com)
 */
public class PathUtility {

    private final static String PATH_SUFFIX = "content.json";

    public static final String SEP = "/";

    private final static String PATH_SUFFIX_WITH_SEP = SEP + PATH_SUFFIX;

    public String fixLocalPath(@Nonnull String realPath) {
        if (realPath == null) {
            return null;
        }

        // ignore paths that end with /
        if (realPath.endsWith(SEP)) {
            return realPath;
        }

        // ignore paths that end with .content
        if (realPath.endsWith(PATH_SUFFIX)) {
            return realPath;
        }

        return realPath + PATH_SUFFIX_WITH_SEP;
    }

    public String unfixRemotePath(@Nonnull String realPath) {
        if (realPath == null) {
            return null;
        }

        if (realPath.endsWith(PATH_SUFFIX_WITH_SEP)) {
            return realPath.substring(0, realPath.length() - PATH_SUFFIX_WITH_SEP.length());
        } else {
            return realPath;
        }
    }
}

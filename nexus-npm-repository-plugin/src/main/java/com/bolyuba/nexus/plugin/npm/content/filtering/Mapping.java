package com.bolyuba.nexus.plugin.npm.content.filtering;

import javax.annotation.Nonnull;
import java.net.MalformedURLException;
import java.net.URL;

/**
 * @author Georgy Bolyuba (georgy@bolyuba.com)
 */
public class Mapping {

    private URL from;

    private URL to;

    public Mapping(@Nonnull String from) {
        try {
            this.from = new URL(from);
        } catch (MalformedURLException e) {
            // this should never happen, all strings are well formed URLs
            throw new RuntimeException(e);
        }
    }

    public Mapping(@Nonnull String from, @Nonnull String to) {
        this(from);

        try {
            this.to = new URL(to);
        } catch (MalformedURLException e) {
            // this should never happen, all strings are well formed URLs
            throw new RuntimeException(e);
        }
    }

    public String map(String s) {
        try {
            URL url = new URL(s);
            if (!from.getHost().equals(url.getHost())) {
                return s;
            }
            if (to == null) {
                return url.getFile();
            } else {
                return new URL(to.getProtocol(), to.getHost(), to.getPort(), to.getFile() + url.getFile()).toString();
            }
        } catch (MalformedURLException e) {
            return s;
        }
    }
}


package com.bolyuba.nexus.plugin.npm.proxy.content;

import com.google.gson.Gson;

import javax.annotation.Nonnull;
import java.net.MalformedURLException;
import java.net.URL;

/**
 * @author Georgy Bolyuba (georgy@bolyuba.com)
 */
public class MappingFilter {

    private final Gson gson = new Gson();

    private boolean valueIdExpected = false;

    private final String key;

    private final Mapping mapping;

    public MappingFilter(@Nonnull String key, @Nonnull Mapping mapping) {
        this.key = key;
        this.mapping = mapping;
    }

    public String aName(String name) {
        valueIdExpected = key.equals(name);
        return "\"" + name + "\":";
    }

    public String beginArray() {
        valueIdExpected = false;
        return "[";
    }

    public String endArray() {
        valueIdExpected = false;
        return "]";
    }

    public String beginObject() {
        valueIdExpected = false;
        return "{";
    }

    public String endObject() {
        valueIdExpected = false;
        return "}";
    }

    public String aString(String s) {
        if (valueIdExpected) {
            s = mapping.map(s);
        }
        valueIdExpected = false;

        return gson.toJson(s);
    }

    public String aNumber(String n) {
        valueIdExpected = false;
        return "\"" + n + "\"";
    }

    public String aBoolean(boolean b) {
        valueIdExpected = false;
        return Boolean.toString(b);
    }

    public String aNull() {
        valueIdExpected = false;
        return "null";
    }
}

class Mapping {

    private URL from;

    private URL to;

    Mapping(@Nonnull String from) {
        try {
            this.from = new URL(from);
        } catch (MalformedURLException e) {
            // this should never happen, all strings are well formed URLs
            throw new RuntimeException(e);
        }
    }

    Mapping(@Nonnull String from, @Nonnull String to) {
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

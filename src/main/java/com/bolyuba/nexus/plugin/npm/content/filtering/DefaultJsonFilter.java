package com.bolyuba.nexus.plugin.npm.content.filtering;

import com.google.gson.Gson;

/**
 * A "pass through" filter that does not change content in any way (except escaping might be different)
 *
 * @author Georgy Bolyuba (georgy@bolyuba.com)
 */
public class DefaultJsonFilter implements JsonFilter {

    private final Gson gson = new Gson();

    @Override
    public String aName(String name) {
        return "\"" + name + "\":";
    }

    @Override
    public String beginArray() {
        return "[";
    }

    @Override
    public String endArray() {
        return "]";
    }

    @Override
    public String beginObject() {
        return "{";
    }

    @Override
    public String endObject() {
        return "}";
    }

    @Override
    public String aString(String s) {
        return gson.toJson(s);
    }

    @Override
    public String aNumber(String n) {
        return "\"" + n + "\"";
    }

    @Override
    public String aBoolean(boolean b) {
        return Boolean.toString(b);
    }

    @Override
    public String aNull() {
        return "null";
    }
}

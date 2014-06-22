package com.bolyuba.nexus.plugin.npm.content.filtering;

import javax.annotation.Nonnull;

/**
 * @author Georgy Bolyuba (georgy@bolyuba.com)
 */
public class JsonMappingFilter extends DefaultJsonFilter {

    private boolean valueIdExpected = false;

    private final String key;

    private final Mapping mapping;

    public JsonMappingFilter(@Nonnull String key, @Nonnull Mapping mapping) {
        this.key = key;
        this.mapping = mapping;
    }

    @Override
    public String aName(String name) {
        valueIdExpected = key.equals(name);
        return super.aName(name);
    }

    @Override
    public String beginArray() {
        valueIdExpected = false;
        return super.beginArray();
    }

    @Override
    public String endArray() {
        valueIdExpected = false;
        return super.endArray();
    }

    @Override
    public String beginObject() {
        valueIdExpected = false;
        return super.beginObject();
    }

    @Override
    public String endObject() {
        valueIdExpected = false;
        return super.endObject();
    }

    @Override
    public String aString(String s) {
        if (valueIdExpected) {
            s = mapping.map(s);
        }
        valueIdExpected = false;
        return super.aString(s);
    }

    @Override
    public String aNumber(String n) {
        valueIdExpected = false;
        return super.aNumber(n);
    }

    @Override
    public String aBoolean(boolean b) {
        valueIdExpected = false;
        return super.aBoolean(b);
    }

    @Override
    public String aNull() {
        valueIdExpected = false;
        return super.aNull();
    }
}
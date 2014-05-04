package com.bolyuba.nexus.plugin.npm.content.filtering;

import javax.annotation.Nonnull;

/**
 * Filter implementation that delegates filtering to next filter in the chain.
 *
 * @author Georgy Bolyuba (georgy@bolyuba.com)
 */
public class ChainableJsonFilter extends DefaultJsonFilter {

    JsonFilter next;

    ChainableJsonFilter() {
    }

    public ChainableJsonFilter(@Nonnull JsonFilter next) {
        this.next = next;
    }

    @Override
    public String aName(String name) {
        return next.aName(name);
    }

    @Override
    public String beginArray() {
        return next.beginArray();
    }

    @Override
    public String endArray() {
        return next.endArray();
    }

    @Override
    public String beginObject() {
        return next.beginObject();
    }

    @Override
    public String endObject() {
        return next.endObject();
    }

    @Override
    public String aString(String s) {
        return next.aString(s);
    }

    @Override
    public String aNumber(String n) {
        return next.aNumber(n);
    }

    @Override
    public String aBoolean(boolean b) {
        return next.aBoolean(b);
    }

    @Override
    public String aNull() {
        return next.aNull();
    }
}

package com.bolyuba.nexus.plugin.npm.content.filtering;

import javax.annotation.Nonnull;

/**
 * Filter implementation that simply delegated processing to another filter. Subclasses can decorate
 * processing as they see fit.
 *
 * @author Georgy Bolyuba (georgy@bolyuba.com)
 */
public class DecoratingJsonFilter extends DefaultJsonFilter {

    JsonFilter delegate;

    DecoratingJsonFilter() {}

    public DecoratingJsonFilter(@Nonnull JsonFilter next) {
        this.delegate = next;
    }

    @Override
    public String aName(String name) {
        return delegate.aName(name);
    }

    @Override
    public String beginArray() {
        return delegate.beginArray();
    }

    @Override
    public String endArray() {
        return delegate.endArray();
    }

    @Override
    public String beginObject() {
        return delegate.beginObject();
    }

    @Override
    public String endObject() {
        return delegate.endObject();
    }

    @Override
    public String aString(String s) {
        return delegate.aString(s);
    }

    @Override
    public String aNumber(String n) {
        return delegate.aNumber(n);
    }

    @Override
    public String aBoolean(boolean b) {
        return delegate.aBoolean(b);
    }

    @Override
    public String aNull() {
        return delegate.aNull();
    }
}

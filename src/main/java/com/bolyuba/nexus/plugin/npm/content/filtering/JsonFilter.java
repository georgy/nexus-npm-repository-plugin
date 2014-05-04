package com.bolyuba.nexus.plugin.npm.content.filtering;

/**
 * @author Georgy Bolyuba (georgy@bolyuba.com)
 */
public interface JsonFilter {

    String aName(String name);

    String beginArray();

    String endArray();

    String beginObject();

    String endObject();

    String aString(String s);

    String aNumber(String n);

    String aBoolean(boolean b);

    String aNull();
}

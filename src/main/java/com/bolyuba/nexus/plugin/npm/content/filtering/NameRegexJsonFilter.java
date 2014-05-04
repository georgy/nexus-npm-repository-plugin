package com.bolyuba.nexus.plugin.npm.content.filtering;

import javax.annotation.Nonnull;
import java.util.regex.Pattern;

/**
 * Filter that suppresses values by name using regex
 *
 * @author Georgy Bolyuba (georgy@bolyuba.com)
 */
public class NameRegexJsonFilter extends ChainableJsonFilter {

    private static final String NOTHING = "";

    private final Pattern pattern;

    private boolean suppersNext;

    public NameRegexJsonFilter(@Nonnull JsonFilter next, @Nonnull String pattern) {
        super(next);
        this.pattern = Pattern.compile(pattern);
    }

    @Override
    public String aName(String name) {
        suppersNext = shouldSuppress(name);
        return suppress(super.aName(name));
    }

    @Override
    public String beginArray() {
        return suppress(super.beginArray());
    }

    @Override
    public String beginObject() {
        return suppress(super.beginObject());
    }

    @Override
    public String endArray() {
        return suppressAndClear(super.endArray());
    }

    @Override
    public String endObject() {
        return suppressAndClear(super.endObject());
    }

    @Override
    public String aString(String s) {
        return suppressAndClear(super.aString(s));
    }

    @Override
    public String aNumber(String n) {
        return suppressAndClear(super.aNumber(n));
    }

    @Override
    public String aBoolean(boolean b) {
        return suppressAndClear(super.aBoolean(b));
    }

    @Override
    public String aNull() {
        return suppressAndClear(super.aNull());
    }

    private boolean shouldSuppress(String name) {
        return pattern.matcher(name).matches();
    }

    private String suppressAndClear(String value) {
        return checkAndSuppress(value, true);
    }

    private String suppress(String value) {
        return checkAndSuppress(value, false);
    }

    private String checkAndSuppress(String value, boolean clear) {
        if (suppersNext) {
            if (clear) {
                suppersNext = false;
            }
            return NOTHING;
        } else {
            return value;
        }
    }
}

package com.bolyuba.nexus.plugin.npm.content;

import com.google.gson.stream.JsonReader;
import com.google.gson.stream.JsonToken;

import javax.annotation.Nonnull;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;

/**
 * @author Georgy Bolyuba (georgy@bolyuba.com)
 */
public class NpmJsonReader extends JsonReader {

    public NpmJsonReader(InputStream in) {
        super(new InputStreamReader(in));
    }

    /**
     * Skips everything on the same level until finds name. If name is found reader is ready to read
     * next value (might be object, array, number, etc. but not name)
     *
     * @param targetName name we are looking for
     * @return {@code true} if name is found, {@code false} otherwise
     * @throws IOException
     */
    public boolean skipToName(@Nonnull String targetName) throws IOException {

        do {
            JsonToken peek = peek();

            if (peek == JsonToken.BEGIN_OBJECT) {
                beginObject();
                continue;
            }

            if (peek == JsonToken.END_DOCUMENT) {
                return false;
            }

            if (peek != JsonToken.NAME) {
                skipValue();
                continue;
            }

            String name = nextName();

            if (targetName.equals(name.toLowerCase())) {
                return true;
            } else {
                skipValue();
            }
        } while (true);
    }
}

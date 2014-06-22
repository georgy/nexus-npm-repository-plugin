package com.bolyuba.nexus.plugin.npm.content.filtering;

import com.google.gson.stream.JsonReader;
import com.google.gson.stream.JsonToken;

import javax.annotation.Nonnull;
import java.io.FilterInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.nio.ByteBuffer;

import static com.google.common.base.Preconditions.checkNotNull;
import static com.google.gson.stream.JsonToken.END_ARRAY;
import static com.google.gson.stream.JsonToken.END_DOCUMENT;
import static com.google.gson.stream.JsonToken.END_OBJECT;
import static com.google.gson.stream.JsonToken.NULL;

/**
 * An implementation of FilterInputStream that reads JSON and applies
 *
 * @author Georgy Bolyuba (georgy@bolyuba.com)
 */
public class JsonFilterInputStream extends FilterInputStream {

    public static final String CHARSET_NAME = "UTF-8";

    private final JsonReader reader;

    private ByteBuffer buffer;

    private final JsonFilter jsonFilter;

    public JsonFilterInputStream(@Nonnull InputStream in, @Nonnull JsonFilter jsonFilter) throws IOException {
        super(in);
        this.jsonFilter = checkNotNull(jsonFilter);
        reader = new JsonReader(new InputStreamReader(in, CHARSET_NAME));
        fillBuffer("");
    }

    @Override
    public int read() throws IOException {
        if (!buffer.hasRemaining()) {
            if (isStreamEmpty()) {
                // our job here is done
                return -1;
            } else {
                // need to fill up the buffer
                if (!updateBuffer()) {
                    // end of the document
                    return -1;
                }
            }
        }

        return buffer.get();
    }

    @SuppressWarnings("NullableProblems")
    @Override
    public int read(byte[] b) throws IOException {
        return read(b, 0, b.length);
    }

    @SuppressWarnings({"NullableProblems", "EmptyCatchBlock"})
    @Override
    public int read(byte[] b, int off, int len) throws IOException {
        if (b == null) {
            throw new NullPointerException();
        } else if (off < 0 || len < 0 || len > b.length - off) {
            throw new IndexOutOfBoundsException();
        } else if (len == 0) {
            return 0;
        }

        int c = read();
        if (c == -1) {
            return -1;
        }
        b[off] = (byte) c;

        int i = 1;
        try {
            for (; i < len; i++) {
                c = read();
                if (c == -1) {
                    break;
                }
                b[off + i] = (byte) c;
            }
        } catch (IOException e) {
        }
        return i;
    }

    @Override
    public long skip(long n) throws IOException {
        throw new IOException("skip is not supported");
    }

    @Override
    public int available() throws IOException {
        return 0;
    }

    @Override
    public void close() throws IOException {
        reader.close();
        super.close();
    }

    private boolean isStreamEmpty() throws IOException {
        return (reader.peek() == END_DOCUMENT);
    }

    private boolean updateBuffer() throws IOException {
        do {
            JsonToken token = reader.peek();
            switch (token) {
                case END_DOCUMENT:
                    return false;
                case BEGIN_ARRAY:
                    reader.beginArray();
                    fillBuffer(jsonFilter.beginArray());
                    break;
                case END_ARRAY:
                    reader.endArray();
                    String json = jsonFilter.endArray();
                    if (needComma(reader, json)) {
                        json = json + ",";
                    }
                    fillBuffer(json);
                    break;
                case BEGIN_OBJECT:
                    reader.beginObject();
                    fillBuffer(jsonFilter.beginObject());
                    break;
                case END_OBJECT:
                    reader.endObject();
                    json = jsonFilter.endObject();
                    if (needComma(reader, json)) {
                        json = json + ",";
                    }
                    fillBuffer(json);
                    break;
                case NAME:
                    String name = reader.nextName();
                    fillBuffer(jsonFilter.aName(name));
                    break;
                case STRING:
                    String s = reader.nextString();
                    json = jsonFilter.aString(s);
                    if (needComma(reader, json)) {
                        json = json + ",";
                    }
                    fillBuffer(json);
                    break;
                case NUMBER:
                    String n = reader.nextString();
                    json = jsonFilter.aNumber(n);
                    if (needComma(reader, json)) {
                        json = json + ",";
                    }
                    fillBuffer(json);
                    break;
                case BOOLEAN:
                    boolean b = reader.nextBoolean();
                    json = jsonFilter.aBoolean(b);
                    if (needComma(reader, json)) {
                        json = json + ",";
                    }
                    fillBuffer(json);
                    break;
                case NULL:
                    reader.nextNull();
                    json = jsonFilter.aNull();
                    if (needComma(reader, json)) {
                        json = json + ",";
                    }
                    fillBuffer(json);
                    break;
            }

         // tried our best, but buffer is still empty
        } while (!buffer.hasRemaining());
        return true;
    }

    private boolean needComma(JsonReader reader, String json) throws IOException {
        // we might be skipping this element
        if (json.isEmpty()) {
            return false;
        }
        JsonToken peek = reader.peek();
        return ((peek != END_ARRAY) && (peek != END_OBJECT) && (peek != END_DOCUMENT) && (peek != NULL));
    }

    private void fillBuffer(String value) throws IOException {
        this.buffer = ByteBuffer.wrap(value.getBytes(CHARSET_NAME));
    }

    // mark/reset - do nothing
    @Override
    public synchronized void mark(int readlimit) {
    }

    @Override
    public synchronized void reset() throws IOException {
        throw new IOException("mark/reset is not supported");
    }

    @Override
    public boolean markSupported() {
        return false;
    }
}

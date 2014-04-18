package com.bolyuba.nexus.plugin.npm.proxy.content;

import com.google.gson.Gson;
import com.google.gson.stream.JsonReader;
import com.google.gson.stream.JsonToken;

import java.io.*;
import java.nio.ByteBuffer;

import static com.google.gson.stream.JsonToken.END_DOCUMENT;
import static com.google.gson.stream.JsonToken.NAME;

/**
 * @author Georgy Bolyuba (georgy@bolyuba.com)
 */
public class NpmFilterInputStream extends FilterInputStream {

    public static final String CHARSET_NAME = "UTF-8";

    private final JsonReader reader;

    private final Gson gson =  new Gson();

    private ByteBuffer buffer;

    public NpmFilterInputStream(InputStream in) throws IOException {
        super(in);
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
                updateBuffer();
            }
        }

        return buffer.get();
    }

    @Override
    public int read(byte[] b) throws IOException {
        return read(b, 0, b.length);
    }

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
        b[off] = (byte)c;

        int i = 1;
        try {
            for (; i < len ; i++) {
                c = read();
                if (c == -1) {
                    break;
                }
                b[off + i] = (byte)c;
            }
        } catch (IOException ee) {
        }
        return i;
    }

    @Override
    public long skip(long n) throws IOException {
        throw new IOException("skip not supported");
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

    private void updateBuffer() throws IOException {
        JsonToken token = reader.peek();
        switch (token) {
            case BEGIN_ARRAY:
                reader.beginArray();
                fillBuffer("[");
                break;
            case END_ARRAY:
                reader.endArray();
                String json = "]";
                if (needComma(reader)) {
                    json = json + ",";
                }
                fillBuffer(json);
                break;
            case BEGIN_OBJECT:
                reader.beginObject();
                fillBuffer("{");
                break;
            case END_OBJECT:
                reader.endObject();
                json = "}";
                if (needComma(reader)) {
                    json = json + ",";
                }
                fillBuffer(json);                break;
            case NAME:
                String name = reader.nextName();
                fillBuffer(gson.toJson(name) + ":");
                break;
            case STRING:
                String s = reader.nextString();
                json = gson.toJson(s);
                if (needComma(reader)) {
                    json = json + ",";
                }
                fillBuffer(json);
                break;
            case NUMBER:
                String n = reader.nextString();
                json = gson.toJson(n);
                if (needComma(reader)) {
                    json = json + ",";
                }
                fillBuffer(json);
                break;
            case BOOLEAN:
                String b = reader.nextString();
                json = gson.toJson(b);
                if (needComma(reader)) {
                    json = json + ",";
                }
                fillBuffer(json);
                break;
            case NULL:
                reader.nextNull();
                break;
        }
    }

    private boolean needComma(JsonReader reader) throws IOException {
        return reader.peek() == NAME;
    }

    private void fillBuffer(String value) throws IOException {
        ByteBuffer newBuffer = ByteBuffer.wrap(value.getBytes(CHARSET_NAME));
        this.buffer = newBuffer;
    }

    // mark/reset - do nothing

    @Override
    public synchronized void mark(int readlimit) {
    }

    @Override
    public synchronized void reset() throws IOException {
        throw new IOException("mark/reset not supported");
    }

    @Override
    public boolean markSupported() {
        return false;
    }
}

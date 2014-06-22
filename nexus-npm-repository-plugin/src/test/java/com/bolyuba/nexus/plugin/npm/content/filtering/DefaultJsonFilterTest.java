package com.bolyuba.nexus.plugin.npm.content.filtering;

import org.testng.annotations.Test;

import static org.testng.Assert.assertEquals;
import static org.testng.Assert.assertNotNull;

/**
 * @author <a href="mailto:georgy@bolyuba.com">Georgy Bolyuba</a>
 */
public class DefaultJsonFilterTest {

    private DefaultJsonFilter sut = new DefaultJsonFilter();

    @Test
    public void name() {
        String testName = sut.aName("testName");

        assertNotNull(testName);
        assertEquals(testName, "\"testName\":");
    }

    @Test
    public void beginObj() {
        String beginObject = sut.beginObject();

        assertNotNull(beginObject);
        assertEquals(beginObject, "{");
    }

    @Test
    public void endObj() {
        String endObject = sut.endObject();

        assertNotNull(endObject);
        assertEquals(endObject, "}");
    }

    @Test
    public void beginArray() {
        String beginArray = sut.beginArray();

        assertNotNull(beginArray);
        assertEquals(beginArray, "[");
    }

    @Test
    public void endArray() {
        String endArray = sut.endArray();

        assertNotNull(endArray);
        assertEquals(endArray, "]");
    }

    @Test
    public void nil() {
        String val = sut.aNull();

        assertNotNull(val);
        assertEquals(val, "null");
    }

    @Test
    public void one() {
        String val = sut.aBoolean(true);

        assertNotNull(val);
        assertEquals(val, Boolean.TRUE.toString());
    }

    @Test
    public void zero() {
        String val = sut.aBoolean(false);

        assertNotNull(val);
        assertEquals(val, Boolean.FALSE.toString());
    }

    @Test
    public void num() {
        String val = sut.aNumber("42");

        assertNotNull(val);
        assertEquals(val, "\"42\"");
    }

    @Test
    public void str() {
        String val = sut.aString("Answer to life, universe & everything: 42");

        assertNotNull(val);
        assertEquals(val, "\"Answer to life, universe \\u0026 everything: 42\"");
    }

}

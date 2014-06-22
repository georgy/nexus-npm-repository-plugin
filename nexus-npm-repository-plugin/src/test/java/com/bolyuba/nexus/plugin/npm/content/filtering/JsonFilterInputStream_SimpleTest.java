package com.bolyuba.nexus.plugin.npm.content.filtering;

import org.apache.commons.io.IOUtils;
import org.testng.Assert;
import org.testng.annotations.Test;

import java.io.IOException;

/**
 * @author Georgy Bolyuba (georgy@bolyuba.com)
 */
public class JsonFilterInputStream_SimpleTest {

    @Test
    public void test() throws IOException {
        final JsonFilterInputStream inputStream = new JsonFilterInputStream(this.getClass().getResourceAsStream("simple-test-input.json"), new PackageRootOverwritingFilter());
        String result = IOUtils.toString(inputStream, "UTF-8");
        Assert.assertEquals(result, "{\"name\":\"my-app\"}");
    }
}

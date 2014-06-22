package com.bolyuba.nexus.plugin.npm.content.filtering;

import org.apache.commons.io.IOUtils;
import org.testng.Assert;
import org.testng.annotations.Test;

import java.io.IOException;

/**
 * @author Georgy Bolyuba (georgy@bolyuba.com)
 */
public class PackageRootOverwritingFilterTest {

    @Test(enabled = false)
    public void test() throws IOException {

        JsonFilterInputStream inputStream = new JsonFilterInputStream(
                this.getClass().getResourceAsStream(this.getClass().getSimpleName() + "-in.json"),
                new PackageRootOverwritingFilter());

        String result = IOUtils.toString(inputStream, "UTF-8");
        String expected = IOUtils.toString(this.getClass().getResourceAsStream(this.getClass().getSimpleName() + "-out.json"), "UTF-8");


        Assert.assertEquals(result, expected);
    }
}

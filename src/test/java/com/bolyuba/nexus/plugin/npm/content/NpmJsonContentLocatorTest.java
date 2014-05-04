package com.bolyuba.nexus.plugin.npm.content;

import com.bolyuba.nexus.plugin.npm.NpmRepository;
import org.testng.annotations.Test;

import static org.testng.Assert.assertEquals;
import static org.testng.Assert.assertFalse;

/**
 * @author Georgy Bolyuba (georgy@bolyuba.com)
 */
public class NpmJsonContentLocatorTest {

    @Test
    public void test_ok() {
        NpmJsonContentLocator sut = new NpmJsonContentLocator("{}");

        assertEquals(sut.getLength(), 2);
        assertFalse(sut.isReusable());
        assertEquals(sut.getMimeType(), NpmRepository.JSON_MIME_TYPE);
    }
}

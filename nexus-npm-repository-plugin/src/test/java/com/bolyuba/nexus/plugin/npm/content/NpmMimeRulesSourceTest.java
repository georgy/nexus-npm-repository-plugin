package com.bolyuba.nexus.plugin.npm.content;

import org.mockito.MockitoAnnotations;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

import static org.testng.Assert.assertEquals;
import static org.testng.Assert.assertNotNull;
import static org.testng.Assert.assertNull;

/**
 * @author Georgy Bolyuba (georgy@bolyuba.com)
 */
public class NpmMimeRulesSourceTest {


    private NpmMimeRulesSource sut;

    @BeforeMethod
    void setup () {
        MockitoAnnotations.initMocks(this);

        sut = new NpmMimeRulesSource();
    }

    @Test
    public void nullPath_noSuggestion() {
        String ruleForPath = sut.getRuleForPath(null);

        assertNull(ruleForPath);
    }

    @Test
    public void randomPath_noSuggestion() {
        String ruleForPath = sut.getRuleForPath("/random/path");

        assertNull(ruleForPath);
    }

    @Test
    public void packageRootContent_JsonMimeType() {
        String ruleForPath = sut.getRuleForPath("/package/-content.json");

        assertNotNull(ruleForPath);
        assertEquals(ruleForPath, "application/json");
    }

    @Test
    public void packageVersionContent_JsonMimeType() {
        String ruleForPath = sut.getRuleForPath("/package/42.42.42/-content.json");

        assertNotNull(ruleForPath);
        assertEquals(ruleForPath, "application/json");
    }
}

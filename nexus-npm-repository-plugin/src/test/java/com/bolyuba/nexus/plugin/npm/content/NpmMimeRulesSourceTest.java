package com.bolyuba.nexus.plugin.npm.content;

import com.bolyuba.nexus.plugin.npm.NpmRepository;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

import static org.testng.Assert.assertEquals;
import static org.testng.Assert.assertNotNull;
import static org.testng.Assert.assertNull;

/**
 * @author Georgy Bolyuba (georgy@bolyuba.com)
 */
public class NpmMimeRulesSourceTest
{
  private NpmMimeRulesSource sut;

  @BeforeMethod
  void setup() {
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

    assertNotNull(ruleForPath);
    assertEquals(ruleForPath, NpmRepository.JSON_MIME_TYPE);
  }

  @Test
  public void packageRootContent_JsonMimeType() {
    String ruleForPath = sut.getRuleForPath("/package/-content.json");

    assertNull(ruleForPath);
  }

  @Test
  public void packageVersionContent_JsonMimeType() {
    String ruleForPath = sut.getRuleForPath("/package/42.42.42/-content.json");

    assertNull(ruleForPath);
  }

  @Test
  public void tarball_TarballMimeType() {
    String ruleForPath = sut.getRuleForPath("/package/-/package-1.0.0.tgz");

    assertNotNull(ruleForPath);
    assertEquals(ruleForPath, NpmRepository.TARBALL_MIME_TYPE);
  }
}

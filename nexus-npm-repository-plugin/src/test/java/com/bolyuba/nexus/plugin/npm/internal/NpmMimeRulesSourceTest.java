package com.bolyuba.nexus.plugin.npm.internal;

import org.sonatype.sisu.litmus.testsupport.TestSupport;

import com.bolyuba.nexus.plugin.npm.NpmRepository;
import org.junit.Test;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.notNullValue;
import static org.hamcrest.Matchers.nullValue;

/**
 * @author Georgy Bolyuba (georgy@bolyuba.com)
 */
public class NpmMimeRulesSourceTest
    extends TestSupport
{
  private NpmMimeRulesSource sut = new NpmMimeRulesSource();

  @Test
  public void nullPath_noSuggestion() {
    String ruleForPath = sut.getRuleForPath(null);

    assertThat(ruleForPath, nullValue());
  }

  @Test
  public void randomPath_noSuggestion() {
    String ruleForPath = sut.getRuleForPath("/random/path");

    assertThat(ruleForPath, notNullValue());
    assertThat(ruleForPath, equalTo(NpmRepository.JSON_MIME_TYPE));
  }

  @Test
  public void packageRootContent_JsonMimeType() {
    String ruleForPath = sut.getRuleForPath("/package/-content.json");

    assertThat(ruleForPath, nullValue());
  }

  @Test
  public void packageVersionContent_JsonMimeType() {
    String ruleForPath = sut.getRuleForPath("/package/42.42.42/-content.json");

    assertThat(ruleForPath, nullValue());
  }

  @Test
  public void tarball_TarballMimeType() {
    String ruleForPath = sut.getRuleForPath("/package/-/package-1.0.0.tgz");

    assertThat(ruleForPath, notNullValue());
    assertThat(ruleForPath, equalTo(NpmRepository.TARBALL_MIME_TYPE));
  }
}

package com.bolyuba.nexus.plugin.npm.metadata;

import java.util.Map;

import org.sonatype.sisu.litmus.testsupport.TestSupport;

import com.beust.jcommander.internal.Maps;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.SerializationFeature;
import org.junit.Test;

import static org.hamcrest.MatcherAssert.*;
import static org.hamcrest.Matchers.*;

/**
 * UT for {@link PackageRoot}
 */
public class PackageRootTest
    extends TestSupport
{
  private final ObjectMapper objectMapper;

  public PackageRootTest() {
    objectMapper = new ObjectMapper();
    objectMapper.enable(SerializationFeature.INDENT_OUTPUT);
  }

  @Test
  public void overlay() throws Exception{
    final Map<String, Object> commonjs1Map = objectMapper.readValue(util.resolveFile("src/test/npm/ROOT_commonjs_v1.json"), new TypeReference<Map<String, Object>>() {});
    final PackageRoot commonjs1 = new PackageRoot("repo", commonjs1Map);

    final Map<String, Object> commonjs2Map = objectMapper.readValue(util.resolveFile("src/test/npm/ROOT_commonjs_v2.json"), new TypeReference<Map<String, Object>>() {});
    final PackageRoot commonjs2 = new PackageRoot("repo", commonjs2Map);

    final Map<String, Object> commonjs3Map = objectMapper.readValue(util.resolveFile("src/test/npm/ROOT_commonjs_vIncomplete.json"), new TypeReference<Map<String, Object>>() {});
    final PackageRoot commonjs3 = new PackageRoot("repo", commonjs3Map);

    assertThat(commonjs1.getComponentId(), equalTo(commonjs2.getComponentId()));
    assertThat(commonjs1.getVersions().keySet(), hasItem("0.0.1"));
    assertThat(commonjs1.isIncomplete(), is(false));

    commonjs1.overlay(commonjs2);

    assertThat(commonjs1.getComponentId(), equalTo(commonjs2.getComponentId()));
    assertThat(commonjs1.getVersions().keySet(), hasItems("0.0.1", "0.0.2"));
    assertThat(commonjs1.isIncomplete(), is(false));

    commonjs1.overlay(commonjs3);

    assertThat(commonjs1.getVersions().keySet(), hasItems("0.0.1", "0.0.2", "0.0.3"));
    assertThat(commonjs1.isIncomplete(), is(true));

    objectMapper.writeValue(System.out, commonjs1.getRaw());
  }
}

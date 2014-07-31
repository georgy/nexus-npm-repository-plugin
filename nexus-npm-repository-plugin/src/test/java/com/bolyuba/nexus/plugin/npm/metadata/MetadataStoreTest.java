package com.bolyuba.nexus.plugin.npm.metadata;

import java.io.File;
import java.io.FileInputStream;
import java.io.InputStream;
import java.nio.file.Files;
import java.nio.file.StandardCopyOption;
import java.util.zip.GZIPInputStream;

import org.sonatype.nexus.configuration.application.ApplicationDirectories;
import org.sonatype.nexus.proxy.item.ContentLocator;
import org.sonatype.nexus.proxy.item.PreparedContentLocator;
import org.sonatype.nexus.proxy.registry.ContentClass;
import org.sonatype.sisu.litmus.testsupport.TestSupport;

import com.bolyuba.nexus.plugin.npm.NpmRepository;
import com.bolyuba.nexus.plugin.npm.NpmUtility;
import com.bolyuba.nexus.plugin.npm.metadata.internal.MetadataServiceImpl;
import com.bolyuba.nexus.plugin.npm.metadata.internal.orient.OrientMetadataStore;
import com.bolyuba.nexus.plugin.npm.proxy.DefaultNpmProxyRepository;
import com.bolyuba.nexus.plugin.npm.proxy.NpmProxyRepositoryConfigurator;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.mockito.Mock;

import static org.hamcrest.MatcherAssert.*;
import static org.hamcrest.Matchers.*;
import static org.mockito.Matchers.anyString;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

public class MetadataStoreTest
    extends TestSupport
{
  private File tmpDir;

  private NpmRepository npmRepository;

  @Mock
  private ApplicationDirectories applicationDirectories;

  private OrientMetadataStore metadataStore;

  private MetadataService metadataService;

  private MetadataConsumer metadataConsumer;

  private MetadataProducer metadataProducer;

  @Before
  public void setup() throws Exception {
    tmpDir = util.createTempDir();
    when(applicationDirectories.getWorkDirectory(anyString())).thenReturn(tmpDir);

    // not using mock as it would OOM when it tracks invocations, as we work with large files here
    npmRepository = new DefaultNpmProxyRepository(mock(ContentClass.class), mock(
        NpmProxyRepositoryConfigurator.class), mock(NpmUtility.class))
    {
      public String getId() {
        return "repoId";
      }
    };

    metadataStore = new OrientMetadataStore(applicationDirectories);
    metadataService = new MetadataServiceImpl(metadataStore);
    metadataConsumer = metadataService.createConsumer(npmRepository);
    metadataProducer = metadataService.createProducer(npmRepository);

    metadataStore.start();
  }

  @After
  public void teardown() throws Exception {
    metadataStore.stop();
  }

  /**
   * Simple smoke test that pushes _real_ NPM registry root data into the store and then performs some queries
   * against it. This is huge, as we operate on a 40MB JSON file, database will have around 90k entries.
   */
  @Test
  public void registryRootRoundtrip() throws Exception {
    final ContentLocator input = new PreparedContentLocator(
        new GZIPInputStream(new FileInputStream(util.resolveFile("src/test/npm/ROOT_all.json.gz"))),
        NpmRepository.JSON_MIME_TYPE, -1);
    metadataConsumer.consumeRegistryRoot(input);

    log("Splice done");
    // we pushed all into DB, now query
    log(metadataStore.listPackageNames(npmRepository).size());

    final PackageRoot commonjs = metadataStore.getPackageByName(npmRepository, "commonjs");
    log(commonjs.getName() + " || " + commonjs.getVersions().keySet() + "unpublished=" + commonjs.isUnpublished() +
        " incomplete=" + commonjs.isIncomplete());

    final ContentLocator output = metadataProducer.produceRegistryRoot();
    try (InputStream is = output.getContent()) {
      Files.copy(is, new File(tmpDir, "root.json").toPath(), StandardCopyOption.REPLACE_EXISTING);
    }
  }

  /**
   * Simple smoke test that pushes _real_ NPM registry package root and then queries it.
   */
  @Test
  public void registryPackageRootRoundtrip() throws Exception {
    final ContentLocator input = new PreparedContentLocator(
        new FileInputStream(util.resolveFile("src/test/npm/ROOT_commonjs.json")),
        NpmRepository.JSON_MIME_TYPE, -1);
    metadataConsumer.consumePackageRoot(input);

    assertThat(metadataStore.listPackageNames(npmRepository), hasSize(1));

    final PackageRoot commonjs = metadataStore.getPackageByName(npmRepository, "commonjs");
    assertThat(commonjs.getName(), equalTo("commonjs"));
    assertThat(commonjs.isUnpublished(), is(false));
    assertThat(commonjs.isIncomplete(), is(false));

    final PackageVersion commonjs_0_0_1 = commonjs.getVersions().get("0.0.1");
    assertThat(commonjs_0_0_1, notNullValue());
    assertThat(commonjs_0_0_1.getName(), equalTo("commonjs"));
    assertThat(commonjs_0_0_1.getVersion(), equalTo("0.0.1"));
    assertThat(commonjs_0_0_1.isIncomplete(), is(false));
  }

}

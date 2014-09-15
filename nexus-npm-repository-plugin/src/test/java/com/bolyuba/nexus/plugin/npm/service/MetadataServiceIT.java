package com.bolyuba.nexus.plugin.npm.service;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.nio.file.StandardCopyOption;
import java.util.List;
import java.util.zip.GZIPInputStream;

import org.sonatype.nexus.apachehttpclient.Hc4Provider;
import org.sonatype.nexus.configuration.application.ApplicationDirectories;
import org.sonatype.nexus.proxy.ResourceStoreRequest;
import org.sonatype.nexus.proxy.item.ContentLocator;
import org.sonatype.nexus.proxy.item.PreparedContentLocator;
import org.sonatype.nexus.proxy.item.RepositoryItemUid;
import org.sonatype.nexus.proxy.item.RepositoryItemUidLock;
import org.sonatype.nexus.proxy.item.StringContentLocator;
import org.sonatype.nexus.proxy.registry.ContentClass;
import org.sonatype.nexus.proxy.repository.ProxyRepository;
import org.sonatype.nexus.proxy.repository.Repository;
import org.sonatype.nexus.proxy.storage.remote.RemoteStorageContext;
import org.sonatype.nexus.proxy.storage.remote.httpclient.HttpClientManager;
import org.sonatype.nexus.web.BaseUrlHolder;
import org.sonatype.sisu.litmus.testsupport.TestSupport;

import com.bolyuba.nexus.plugin.npm.NpmRepository;
import com.bolyuba.nexus.plugin.npm.group.DefaultNpmGroupRepository;
import com.bolyuba.nexus.plugin.npm.group.NpmGroupRepository;
import com.bolyuba.nexus.plugin.npm.group.NpmGroupRepositoryConfigurator;
import com.bolyuba.nexus.plugin.npm.hosted.DefaultNpmHostedRepository;
import com.bolyuba.nexus.plugin.npm.hosted.NpmHostedRepository;
import com.bolyuba.nexus.plugin.npm.hosted.NpmHostedRepositoryConfigurator;
import com.bolyuba.nexus.plugin.npm.proxy.DefaultNpmProxyRepository;
import com.bolyuba.nexus.plugin.npm.proxy.NpmProxyRepository;
import com.bolyuba.nexus.plugin.npm.proxy.NpmProxyRepositoryConfigurator;
import com.bolyuba.nexus.plugin.npm.service.internal.MetadataParser;
import com.bolyuba.nexus.plugin.npm.service.internal.MetadataServiceFactoryImpl;
import com.bolyuba.nexus.plugin.npm.service.internal.PackageRootIterator;
import com.bolyuba.nexus.plugin.npm.service.internal.ProxyMetadataTransport;
import com.bolyuba.nexus.plugin.npm.service.internal.orient.OrientMetadataStore;
import com.bolyuba.nexus.plugin.npm.service.internal.proxy.HttpProxyMetadataTransport;
import com.bolyuba.nexus.plugin.npm.service.tarball.TarballSource;
import com.bolyuba.nexus.plugin.npm.service.tarball.internal.HttpTarballTransport;
import com.bolyuba.nexus.plugin.npm.service.tarball.internal.Sha1HashPayloadValidator;
import com.bolyuba.nexus.plugin.npm.service.tarball.internal.SizePayloadValidator;
import com.bolyuba.nexus.plugin.npm.service.tarball.internal.TarballSourceImpl;
import com.bolyuba.nexus.plugin.npm.service.tarball.internal.TarballValidator;
import com.google.common.base.Charsets;
import com.google.common.collect.ImmutableMap;
import com.google.common.collect.Lists;
import com.google.common.io.ByteSource;
import com.google.common.io.Files;
import org.apache.http.client.HttpClient;
import org.apache.http.impl.client.HttpClients;
import org.json.JSONObject;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.skyscreamer.jsonassert.JSONAssert;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.greaterThan;
import static org.hamcrest.Matchers.hasSize;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.notNullValue;
import static org.mockito.Matchers.any;
import static org.mockito.Matchers.anyString;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

public class MetadataServiceIT
    extends TestSupport
{
  private File tmpDir;

  private NpmHostedRepository npmHostedRepository;

  private NpmProxyRepository npmProxyRepository;

  private NpmGroupRepository npmGroupRepository;

  @Mock
  private ApplicationDirectories applicationDirectories;

  @Mock
  private Hc4Provider hc4Provider;

  @Mock
  private HttpClientManager httpClientManager;

  private OrientMetadataStore metadataStore;

  private MetadataServiceFactoryImpl metadataService;

  private MetadataParser metadataParser;

  private ProxyMetadataTransport proxyMetadataTransport;

  private TarballSource tarballSource;

  @Before
  public void setup() throws Exception {
    BaseUrlHolder.set("http://localhost:8081/nexus");
    tmpDir = util.createTempDir();

    final HttpClient httpClient = HttpClients.createDefault();

    when(applicationDirectories.getWorkDirectory(anyString())).thenReturn(tmpDir);
    when(applicationDirectories.getTemporaryDirectory()).thenReturn(tmpDir);
    when(httpClientManager.create(any(ProxyRepository.class), any(RemoteStorageContext.class))).thenReturn(
        httpClient);

    metadataStore = new OrientMetadataStore(applicationDirectories);
    metadataParser = new MetadataParser(applicationDirectories.getTemporaryDirectory());
    // proxy transport but without root fetch, to not harrass registry and make tests dead slow
    proxyMetadataTransport = new HttpProxyMetadataTransport(metadataParser, httpClientManager)
    {
      @Override
      public PackageRootIterator fetchRegistryRoot(final NpmProxyRepository npmProxyRepository) throws IOException {
        return PackageRootIterator.EMPTY;
      }
    };
    metadataService = new MetadataServiceFactoryImpl(metadataStore, metadataParser, proxyMetadataTransport);

    when(hc4Provider.createHttpClient(Mockito.any(RemoteStorageContext.class))).thenReturn(httpClient);

    final HttpTarballTransport httpTarballTransport = new HttpTarballTransport(hc4Provider);

    tarballSource = new TarballSourceImpl(applicationDirectories, httpTarballTransport,
        ImmutableMap.<String, TarballValidator>of(
            "size", new SizePayloadValidator(), "sha1", new Sha1HashPayloadValidator()));

    // dummy uid and uidLock
    final RepositoryItemUid uid = mock(RepositoryItemUid.class);
    when(uid.getLock()).thenReturn(mock(RepositoryItemUidLock.class));

    // not using mock as it would OOM when it tracks invocations, as we work with large files here
    npmHostedRepository = new DefaultNpmHostedRepository(mock(ContentClass.class), mock(
        NpmHostedRepositoryConfigurator.class), metadataService)
    {
      @Override
      public String getId() {
        return "hosted";
      }

      @Override
      public RepositoryItemUid createUid(String path) { return uid; }
    };

    // not using mock as it would OOM when it tracks invocations, as we work with large files here
    npmProxyRepository = new DefaultNpmProxyRepository(mock(ContentClass.class), mock(
        NpmProxyRepositoryConfigurator.class), metadataService, tarballSource)
    {
      @Override
      public String getId() {
        return "proxy";
      }

      @Override
      public boolean isItemAgingActive() { return true; }

      @Override
      public int getItemMaxAge() { return 10; }

      @Override
      public String getRemoteUrl() { return "http://registry.npmjs.org/"; }

      @Override
      public RepositoryItemUid createUid(String path) { return uid; }
    };

    // not using mock as it would OOM when it tracks invocations, as we work with large files here
    npmGroupRepository = new DefaultNpmGroupRepository(mock(ContentClass.class), mock(
        NpmGroupRepositoryConfigurator.class), metadataService)
    {
      @Override
      public String getId() {
        return "hosted";
      }

      @Override
      public List<Repository> getMemberRepositories() {
        final List<Repository> result = Lists.newArrayList();
        result.add(npmHostedRepository);
        result.add(npmProxyRepository);
        return result;
      }
    };

    metadataStore.start();
  }

  @After
  public void teardown() throws Exception {
    metadataStore.stop();
    BaseUrlHolder.unset();
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
    // this is "illegal" case using internal stuff, but is for testing only
    metadataStore
        .updatePackages(npmProxyRepository, metadataParser.parseRegistryRoot(npmProxyRepository.getId(), input));

    log("Splice done");
    // we pushed all into DB, now query
    log(metadataStore.listPackageNames(npmProxyRepository).size());

    final PackageRoot commonjs = metadataStore.getPackageByName(npmProxyRepository, "commonjs");
    log(commonjs.getName() + " || " + commonjs.getVersions().keySet() + "unpublished=" + commonjs.isUnpublished() +
        " incomplete=" + commonjs.isIncomplete());

    final ContentLocator output = npmProxyRepository.getMetadataService().getProducer().produceRegistryRoot(
        new PackageRequest(new ResourceStoreRequest("/", true, false)));
    try (InputStream is = output.getContent()) {
      java.nio.file.Files.copy(is, new File(tmpDir, "root.json").toPath(), StandardCopyOption.REPLACE_EXISTING);
    }
  }

  @Test
  public void proxyPackageRootRoundtrip() throws Exception {
    // this call will get it from remote, store, and return it as raw stream
    final StringContentLocator contentLocator = (StringContentLocator) npmProxyRepository.getMetadataService()
        .getProducer().producePackageVersion(new PackageRequest(new ResourceStoreRequest("/commonjs/0.0.1")));
    JSONObject proxiedV001 = new JSONObject(
        ByteSource.wrap(contentLocator.getByteArray()).asCharSource(Charsets.UTF_8).read());

    // get the one from file
    final File jsonFile = util.resolveFile("src/test/npm/ROOT_commonjs.json");
    JSONObject onDisk = new JSONObject(Files.toString(jsonFile, Charsets.UTF_8));
    onDisk.getJSONObject("versions").getJSONObject("0.0.1").getJSONObject("dist").put("tarball",
        "http://localhost:8081/nexus/content/repositories/proxy/commonjs/-/commonjs-0.0.1.tgz");
    JSONObject versions = onDisk.getJSONObject("versions");
    JSONObject diskV001 = versions.getJSONObject("0.0.1");
    diskV001.remove("_id"); // TODO: See MetadataGenerator#filterPackageVersion
    diskV001.remove("_rev"); // TODO: See MetadataGenerator#filterPackageVersion

    JSONAssert.assertEquals(diskV001, proxiedV001, false);
  }

  /**
   * Simple smoke test that pushes _real_ NPM registry package root and then queries it, like in a hosted repository.
   */
  @Test
  public void hostedPackageRootRoundtrip() throws Exception {
    final File jsonFile = util.resolveFile("src/test/npm/ROOT_commonjs.json");
    final ContentLocator input = new PreparedContentLocator(
        new FileInputStream(jsonFile),
        NpmRepository.JSON_MIME_TYPE, -1);
    final PackageRequest request = new PackageRequest(new ResourceStoreRequest("/commonjs"));
    npmHostedRepository.getMetadataService()
        .consumePackageRoot(npmHostedRepository.getMetadataService().parsePackageRoot(
            request, input));

    assertThat(metadataStore.listPackageNames(npmHostedRepository), hasSize(1));

    final PackageRoot commonjs = metadataStore.getPackageByName(npmHostedRepository, "commonjs");
    assertThat(commonjs.getName(), equalTo("commonjs"));
    assertThat(commonjs.isUnpublished(), is(false));
    assertThat(commonjs.isIncomplete(), is(false));

    final PackageVersion commonjs_0_0_1 = commonjs.getVersions().get("0.0.1");
    assertThat(commonjs_0_0_1, notNullValue());
    assertThat(commonjs_0_0_1.getName(), equalTo("commonjs"));
    assertThat(commonjs_0_0_1.getVersion(), equalTo("0.0.1"));
    assertThat(commonjs_0_0_1.isIncomplete(), is(false));

    JSONObject onDisk = new JSONObject(Files.toString(jsonFile, Charsets.UTF_8));
    onDisk.remove("_attachments");
    onDisk.remove("_id"); // TODO: See MetadataGenerator#filterPackageVersion
    onDisk.remove("_rev"); // TODO: See MetadataGenerator#filterPackageVersion
    onDisk.getJSONObject("versions").getJSONObject("0.0.1")
        .remove("_id"); // TODO: See MetadataGenerator#filterPackageVersion
    onDisk.getJSONObject("versions").getJSONObject("0.0.1")
        .remove("_rev"); // TODO: See MetadataGenerator#filterPackageVersion
    onDisk.getJSONObject("versions").getJSONObject("0.0.1").getJSONObject("dist")
        .put("tarball", "http://localhost:8081/nexus/content/repositories/hosted/commonjs/-/commonjs-0.0.1.tgz");
    final StringContentLocator contentLocator = (StringContentLocator) npmHostedRepository.getMetadataService()
        .getProducer().producePackageRoot(new PackageRequest(new ResourceStoreRequest("/commonjs")));
    JSONObject onStore = new JSONObject(
        ByteSource.wrap(contentLocator.getByteArray()).asCharSource(Charsets.UTF_8).read());

    JSONAssert.assertEquals(onDisk, onStore, false);
  }

  /**
   * Simple smoke test that checks group functionality, it should aggregate members.
   */
  @Test
  public void groupPackageRootRoundtrip() throws Exception {
    // deploy private project to hosted repo
    {
      final File jsonFile = util.resolveFile("src/test/npm/ROOT_testproject.json");
      final ContentLocator input = new PreparedContentLocator(
          new FileInputStream(jsonFile),
          NpmRepository.JSON_MIME_TYPE, -1);
      final PackageRequest request = new PackageRequest(new ResourceStoreRequest("/testproject"));
      npmHostedRepository.getMetadataService()
          .consumePackageRoot(npmHostedRepository.getMetadataService().parsePackageRoot(
              request, input));
    }

    // proxy is set up against registry.npmjs.org, so no need to seed it

    // verify we have all what registry.mpmjs.org has + testproject
    final PackageRoot commonjs = npmGroupRepository.getMetadataService()
        .generatePackageRoot(new PackageRequest(new ResourceStoreRequest("/commonjs")));
    assertThat(commonjs, notNullValue());

    final PackageRoot testproject = npmGroupRepository.getMetadataService()
        .generatePackageRoot(new PackageRequest(new ResourceStoreRequest("/testproject")));
    assertThat(testproject, notNullValue());

    final PackageRootIterator iterator = npmGroupRepository.getMetadataService().generateRegistryRoot(
        new PackageRequest(new ResourceStoreRequest("/", true, false)));
    boolean found = false;
    int count = 0;
    while (iterator.hasNext()) {
      PackageRoot root = iterator.next();
      if ("testproject".equals(root.getName())) {
        found = true;
      }
      count++;
    }
    assertThat(count, greaterThan(1)); // we have ALL from registry.npmjs.org + testproject
    assertThat(found, is(true)); // we need to have testproject in there
  }
}

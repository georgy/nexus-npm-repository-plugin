/*
 * Copyright (c) 2014 Georgy Bolyuba, Inc. All rights reserved.
 *
 * This program is licensed to you under the Apache License Version 2.0,
 * and you may not use this file except in compliance with the Apache License Version 2.0.
 * You may obtain a copy of the Apache License Version 2.0 at http://www.apache.org/licenses/LICENSE-2.0.
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the Apache License Version 2.0 is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the Apache License Version 2.0 for the specific language governing permissions and limitations there under.
 */
package com.bolyuba.nexus.plugin.npm.service.internal.orient;

import com.google.common.io.Files;
import com.orientechnologies.orient.core.config.OGlobalConfiguration;
import org.apache.commons.io.FileUtils;
import org.junit.After;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;
import org.sonatype.nexus.configuration.application.ApplicationDirectories;

import java.io.File;
import java.io.IOException;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

/**
 * @author <a href="mailto:georgy@bolyuba.com">Georgy Bolyuba</a>
 */
public class OrientMetadataStoreTest {

    public static final String DB_PATH = "db/npm";

    private File tempDir;

    private File dbFile;

    private ApplicationDirectories applicationDirectories;

    private OrientMetadataStore sut;

    @BeforeClass
    public static void scene() {
        // we will be opening and closing db for each test using temporary folders. need to close storage on db.close()
        OGlobalConfiguration.STORAGE_KEEP_OPEN.setValue(Boolean.FALSE);
    }

    @Before
    public void setup() throws IOException {
        tempDir = Files.createTempDir();
        dbFile = new File(tempDir.getAbsoluteFile() + "/" + DB_PATH);

        applicationDirectories = mock(ApplicationDirectories.class);
        when(applicationDirectories.getWorkDirectory(DB_PATH)).thenReturn(dbFile);

        sut = new OrientMetadataStore(applicationDirectories, 1, 42);
    }

    @Test
    public void onStart_shouldCreateDB() throws Exception {
        assertFalse(dbFile.exists());

        sut.doStart();

        assertTrue(dbFile.exists());
    }

    @Test
    public void onStart_canOpenExistingDB() throws Exception {
        assertFalse(dbFile.exists());

        sut.doStart();
        sut.doStop();

        sut.doStart();

        assertTrue(dbFile.exists());
    }

    @After
    public void cleanup() throws IOException {
        FileUtils.deleteDirectory(tempDir);
    }
}

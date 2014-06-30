package org.sonatype.nexus.testsuite.npm;

import org.junit.Test;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.equalTo;

/**
 * Smoke IT for NPM plugin.
 */
public class SmokeNpmIT
    extends NpmITSupport
{
    public SmokeNpmIT( String nexusBundleCoordinates )
    {
        super( nexusBundleCoordinates );
    }

    @Test
    public void smoke()
    {
        assertThat( client().getNexusStatus().getEditionShort(), equalTo( "OSS" ) );
    }
}
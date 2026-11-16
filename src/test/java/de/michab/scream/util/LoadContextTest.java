/*
 * Scream @ https://github.com/urschleim/scream
 *
 * Copyright © 1998-2023 Michael G. Binz
 */

package de.michab.scream.util;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.File;
import java.net.URI;
import java.net.URL;

import org.junit.jupiter.api.Test;

public class LoadContextTest
{
    @Test
    public void urlJar() throws Exception
    {
        URL u = URI.create(
                "jar:http://www.oreilly.com/javaio.jar!/com/elharo/io/StreamCopier.class").toURL();

        LoadContext lc = new LoadContext( u );

        assertEquals(
                u.toExternalForm(),
                lc.toString() );
        assertEquals(
                new File( "/com/elharo/io/StreamCopier.class" ),
                lc.getFile() );
    }

    @Test
    public void urlFile() throws Exception
    {
        URL u = URI.create(
                "file:/Users/micbinz/git/github/app_scream/target/classes/de/michab/scream/extensions/basic.s").toURL();

        LoadContext lc = new LoadContext( u );

        assertEquals(
                u.toExternalForm(),
                lc.toString() );
    }

    @Test
    public void plainFile() throws Exception
    {
        String u =
                "/Users/micbinz/git/github/app_scream/target/classes/de/michab/scream/extensions/basic.s";

        LoadContext lc = new LoadContext( u );

        assertEquals(
                "file:" + u,
                lc.toString() );
        assertEquals(
                new File( u ),
                lc.getFile() );
    }

    @Test
    public void relativeFile() throws Exception
    {
        String a =
                "/de/michab/scream/extensions/a.s";
        LoadContext lca = new LoadContext( a );
        assertTrue( lca.isAbsolute() );
        assertTrue( lca.isFile() );

        String b =
                "b.s";
        LoadContext lcb = new LoadContext( b );
        assertFalse( lcb.isAbsolute() );

        LoadContext lcc = lcb.relate( lca );
        assertTrue( lcc.isFile() );
        assertEquals(
                new File( "/de/michab/scream/extensions/b.s" ),
                lcc.getFile() );

        LoadContext lcd = new LoadContext( "/tmp/313.so" );
        assertTrue( lcd.isAbsolute() );

        LoadContext lcf = lcd.relate( lcc );
        assertTrue( lcf.isFile() );
        assertEquals(
                new File("/tmp/313.so"),
                lcf.getFile() );
    }

    @Test
    public void relativeFile2() throws Exception
    {
        String a =
                "file:/de/michab/scream/extensions/a.s";
        LoadContext lca = new LoadContext( a );
        assertTrue( lca.isAbsolute() );
        assertTrue( lca.isFile() );

        String b =
                "b.s";
        LoadContext lcb = new LoadContext( b );
        assertFalse( lcb.isAbsolute() );

        LoadContext lcc = lcb.relate( lca );
        assertTrue( lcc.isAbsolute() );
        assertTrue( lcc.isFile() );
        assertEquals(
                new File( "/de/michab/scream/extensions/b.s" ),
                lcc.getFile() );
    }

    @Test
    public void relativeJarHttp() throws Exception
    {
        URL a = URI.create(
                "jar:http://www.oreilly.com/javaio.jar!/com/elharo/io/A.class").toURL();
        LoadContext lca = new LoadContext( a );
        assertTrue( lca.isAbsolute() );
        assertFalse( lca.isFile() );

        String b =
                "B.class";
        LoadContext lcb = new LoadContext( b );
        assertFalse( lcb.isAbsolute() );

        LoadContext lcbr = lcb.relate( lca );
        assertFalse( lcbr.isFile() );
        assertEquals(
                new File( "/com/elharo/io/B.class" ),
                lcbr.getFile() );
        assertEquals(
                "jar:http://www.oreilly.com/javaio.jar!/com/elharo/io/B.class",
                lcbr.toString() );
    }

    @Test
    public void relativeJarFile() throws Exception
    {
        URL a = URI.create(
                "jar:file:/Users/micbinz/git/github/app_scream/target/app_scream-0.0.1-SNAPSHOT.jar!/de/michab/scream/extensions/common-init.s").toURL();
        LoadContext lca = new LoadContext( a );
        assertTrue( lca.isAbsolute() );
        assertFalse( lca.isFile() );

        String b =
                "basic.s";
        LoadContext lcb = new LoadContext( b );
        assertFalse( lcb.isAbsolute() );

        LoadContext lcbr = lcb.relate( lca );
        assertFalse( lcbr.isFile() );
        assertEquals(
                new File( "/de/michab/scream/extensions/basic.s" ),
                lcbr.getFile() );
        assertEquals(
                "jar:file:/Users/micbinz/git/github/app_scream/target/app_scream-0.0.1-SNAPSHOT.jar!/de/michab/scream/extensions/basic.s",
                lcbr.toString() );
    }

//    WARNUNG: stack.peek()= isFile=false
//    Feb. 22, 2023 8:52:17 PM de.michab.scream.Scream load
//    WARNUNG: current=jar:file:/Users/micbinz/git/github/app_scream/target/app_scream-0.0.1-SNAPSHOT.jar!/de/michab/scream/extensions/basic.s isFile=true
//    Feb

}

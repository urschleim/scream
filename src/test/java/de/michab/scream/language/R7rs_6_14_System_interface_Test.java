/*
 * Scream @ https://github.com/urschleim/scream
 *
 * Copyright © 2023-2024 Michael G. Binz
 */
package de.michab.scream.language;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;

import java.io.File;
import java.io.FileWriter;

import org.junit.jupiter.api.Test;

import de.michab.scream.RuntimeX.Code;
import de.michab.scream.ScreamBaseTest;
import de.michab.scream.fcos.Bool;
import de.michab.scream.fcos.Cons;
import de.michab.scream.fcos.SchemeString;
import de.michab.scream.fcos.Vector;
import de.michab.scream.util.Scut;

/**
 * rsr7 6.14 System interface, p59
 *
 * @author micbinz
 */
public class R7rs_6_14_System_interface_Test extends ScreamBaseTest
{
    static String TEST_FILENAME = "screamTest.tmp";

    @Test
    public void load() throws Exception
    {
        long ctm = System.currentTimeMillis();
        File testfile = new File( getClass().getSimpleName() + ".load.s" );

        try
        {
            var writer = new FileWriter( testfile );
            writer.write( "(define __load__ " + ctm + ")\n" );
            writer.close();

            var filename = testfile.getName();

            var t = makeTester();

            t.execute(
                    String.format( "(load \"%s\")", filename ) );
            t.expectFco(
                    "__load__",
                    i(ctm) );
            t.expectError(
                    String.format( "(load \"x%s\")", filename ),
                    Code.IO_ERROR );
        }
        finally
        {
            testfile.delete();
        }
    }

    /**
     * This currently tests all offered functions for files.
     *
     * @throws Exception
     */
    @Test
    public void file_$general() throws Exception
    {
        File testfile = new File( TEST_FILENAME );

        if ( testfile.exists() )
            testfile.delete();

        try {

        var t = makeTester();

        Vector files1 =
                t.execute( "(scream:files:list)", Vector.class );

        t.expectFco(
                String.format( "(file-exists? \"%s\")", TEST_FILENAME ),
                Bool.F );
        t.expectFco(
                String.format( "(scream:files:create \"%s\")", TEST_FILENAME ),
                Bool.T );
        t.expectFco(
                String.format( "(file-exists? \"%s\")", TEST_FILENAME ),
                Bool.T );

        Vector files2 =
                t.execute( "(scream:files:list)", Vector.class );
        assertEquals( files1.size() + 1, files2.size() );

        t.expectFco(
                String.format( "(delete-file \"%s\")", TEST_FILENAME ),
                Bool.T );
        t.expectFco(
                String.format( "(file-exists? \"%s\")", TEST_FILENAME ),
                Bool.F );

        }
        finally
        {
            if ( testfile.exists() )
                testfile.delete();
        }
    }

    @Test
    public void get_environment_variable() throws Exception
    {
        var t = makeTester();

        var val = t.execute(
                "(get-environment-variable \"JAVA_HOME\")" );
        assertInstanceOf( SchemeString.class, val );

        t.expectFco(
                "(get-environment-variable \"_undefined_variable_\")",
                Bool.F );
    }

    @Test
    public void get_environment_variables() throws Exception
    {
        var t = makeTester();

        var val = Scut.as( Cons.class, t.execute(
                "(get-environment-variables)" ) );

        val = Scut.as( Cons.class, val.getCar() );

        assertInstanceOf( SchemeString.class, val.getCar() );
        assertInstanceOf( SchemeString.class, val.getCdr() );
    }

    @Test
    public void current_second() throws Exception
    {
        var t = makeTester();

        t.execute(
                "(define t0 (current-second))" );
        t.expectFco(
                "(exact? t0)",
                Bool.F );

        Thread.sleep( 10 );
        t.execute( "(define t1 (current-second))" );
        t.expectFco(
                "(< t0 t1)",
                Bool.T );
    }

    @Test
    public void current_jiffy() throws Exception
    {
        var t = makeTester();

        t.execute(
                "(define t0 (current-jiffy))" );
        t.expectFco(
                "(exact? t0)",
                Bool.T );

        Thread.sleep( 10 );
        t.execute( "(define t1 (current-jiffy))" );

        t.expectFco(
                "(< t0 t1)",
                Bool.T );
    }

    @Test
    public void jiffies_per_second() throws Exception
    {
        expectFco(
                "(jiffies-per-second)",
                i(1000) );
    }
}

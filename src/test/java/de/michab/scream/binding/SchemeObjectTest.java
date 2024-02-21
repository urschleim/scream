/*
 * Scream @ https://github.com/urschleim/scream
 *
 * Copyright © 1998-2024 Michael G. Binz
 */
package de.michab.scream.binding;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;

import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;
import org.smack.util.StringUtil;

import de.michab.scream.RuntimeX.Code;
import de.michab.scream.ScreamBaseTest;
import de.michab.scream.fcos.Bool;
import de.michab.scream.fcos.Operation;
import de.michab.scream.fcos.Procedure;
import de.michab.scream.util.Scut;

public class SchemeObjectTest extends ScreamBaseTest
{
    @Test
    public void exists() throws Exception
    {
        var t = makeTester();

        Scut.as( Operation.class,
                t.execute(
                        "object" ) );
        Scut.as( Procedure.class,
                t.execute(
                        "object?" ) );
        Scut.as( Operation.class,
                t.execute(
                        "make-object" ) );
    }

    @Test
    public void objectq() throws Exception
    {
        var t = makeTester();

        // Class object.
        t.expectFco(
                "(object? (make-object java.lang.StringBuilder))",
                Bool.T );
        // Instance.
        t.expectFco(
                "(object? (make-object (java.lang.StringBuilder)))",
                Bool.T );
        t.expectFco(
                "(object? 1)",
                Bool.F );
        t.expectFco(
                "(object? '())",
                Bool.F );
        t.expectFco(
                "(object? 'symbol)",
                Bool.F );
        t.expectError(
                "(object?)",
                Code.WRONG_NUMBER_OF_ARGUMENTS );
        t.expectError(
                "(object? 1 2)",
                Code.WRONG_NUMBER_OF_ARGUMENTS );
    }

    public static class Cl_StaticMembers
    {
        public static final int
            publicStaticFinalZero = 0;
        protected static final int
            protectedStaticFinalZero = 0;
        public static int
            publicStaticZero = 0;
        public static int get313()
        {
            return 313;
        }
        public static int add( int a, int b )
        {
            return a + b;
        }
    }

    @Test
    public void accessStaticMembers() throws Exception
    {
        var t = makeTester();

        t.execute(
                """
                (define Cl_StaticMembers
                    (make-object de.michab.scream.binding.SchemeObjectTest$Cl_StaticMembers))
                """ );
        t.expectFco(
                "(Cl_StaticMembers publicStaticFinalZero)",
                i(0) );
        t.expectError(
                "(Cl_StaticMembers publicStaticFinalZero 1)",
                Code.CANNOT_MODIFY_CONSTANT );
        t.expectError(
                "(Cl_StaticMembers protectedStaticFinalZero)",
                Code.FIELD_NOT_FOUND );
        t.expectFco(
                "(Cl_StaticMembers publicStaticZero)",
                i(0) );
        t.expectFco(
                "(Cl_StaticMembers publicStaticZero 8)",
                i(8) );
        t.expectFco(
                "(Cl_StaticMembers publicStaticZero)",
                i(8) );
        t.expectFco(
                "(Cl_StaticMembers (get313))",
                i(313) );
        t.expectFco(
                "(Cl_StaticMembers (add 310 3))",
                i(313) );
    }

    @Test
    public void make_object__createClass() throws Exception
    {
        var t = makeTester();

        var result = t.execute(
                """
                (define o
                   (make-object java.lang.StringBuilder))
                o
                """ );

        assertInstanceOf( SchemeObject.class, result );

        Class<?> cl =
                (Class<?>)result.toJava();
        assertEquals(
                "java.lang.StringBuilder",
                cl.getName() );
    }

    @Test
    public void make_object__createInstanceDefault() throws Exception
    {
        var t = makeTester();

        // Simple class with default constructor.
        var result = t.execute(
                """
                (define StringBuilder
                   (make-object (java.lang.StringBuilder)))
                StringBuilder
                """ );

        assertInstanceOf( SchemeObject.class, result );

        t.expectFco(
                "(StringBuilder (toString))",
                str("") );
    }

    @Test
    public void make_object__createInstance() throws Exception
    {
        var t = makeTester();

        // Simple class with default constructor.
        var result = t.execute(
                """
                (define StringBuilder
                   (make-object (java.lang.StringBuilder \"scream\")))
                StringBuilder
                """ );

        assertInstanceOf( SchemeObject.class, result );

        t.expectFco(
                "(StringBuilder (toString))",
                str("scream") );
    }

    @Test
    public void make_object__useInstance() throws Exception
    {
        var t = makeTester();

        // Simple class with default constructor.
        t.execute(
                """
                (define StringBuilder
                   (make-object (java.lang.StringBuilder)))
                """ );
        t.expectFco(
                "(StringBuilder (toString))",
                str("") );
        t.execute(
                "(StringBuilder (append \"scream\"))" );
        t.expectFco(
                "(StringBuilder (toString))",
                str( "scream" ) );
        t.execute(
                "(StringBuilder (append 313.))" );
        t.expectFco(
                "(StringBuilder (toString))",
                str( "scream313.0" ) );
    }

    @Test
    public void make_object__useInstance_generic() throws Exception
    {
        var t = makeTester();

        // Generic class without default constructor.
        t.execute(
                """
                (define o
                   (make-object (org.smack.util.Pair "stan" "ollie")))
                """ );
        t.expectFco(
                "(o left)",
                str("stan") );
        t.expectFco(
                "(o right)",
                str("ollie") );

        // Generic class without default constructor.
        t.execute(
                """
                (define o
                   (make-object (org.smack.util.Pair 121 313)))
                """ );
        t.expectFco(
                "(o left)",
                i(121) );
        t.expectFco(
                "(o right)",
                i(313) );

        t.execute(
                """
                (define o
                   (make-object (org.smack.util.Pair "stan" 313)))
                """ );
        t.expectFco(
                "(o left)",
                str("stan") );
        t.expectFco(
                "(o right)",
                i(313) );

        t.execute(
                """
                (define o
                   (make-object (org.smack.util.Pair 'lambda 313)))
                """ );
        // Symbol converted to string.
        t.expectFco(
                "(o left)",
                str("lambda") );
        t.expectFco(
                "(o right)",
                i(313) );

        // Storing a function in the pair.
        t.execute(
                """
                (define o
                   (make-object (org.smack.util.Pair + 313)))
                """ );
        t.expectFco(
                "((o left) (o right) (o right))",
                i(626) );
    }


    @Test
    @Disabled
    public void todo_StringBuilder_useInstance() throws Exception
    {
        var t = makeTester();

        // Simple class with default constructor.
        t.execute(
                """
                (define StringBuilder
                   (make-object (java.lang.StringBuilder)))
                """ );
        t.expectFco(
                "(StringBuilder (toString))",
                str("") );
        t.execute(
                "(StringBuilder (append 313))" );

        // Wrong method selected: Returned value is 313.0.
        t.expectFco(
                "(StringBuilder (toString))",
                str( "313" ) );
    }

    public static class MakeInstance
    {
        public String message = StringUtil.EMPTY_STRING;

        public MakeInstance()
        {
        }
        public MakeInstance( String s )
        {
            message = "String:" + s;
        }
        public MakeInstance( String s1, String s2 )
        {
            message = "String:" + s1 + ":String:" + s2;
        }
        public MakeInstance( float f )
        {
            message = "float:" + f;
        }
        public MakeInstance( Float f )
        {
            message = "Float:" + f;
        }
        public MakeInstance( double d )
        {
            message = "double:" + d;
        }
        public MakeInstance( float f, double d )
        {
            message = "float:" + f + ":double:" + d;
        }
        public MakeInstance( double d, float f )
        {
            message = "double:" + d + ":float:" + f;
        }
        public MakeInstance( boolean b )
        {
            message = "boolean:" + b;
        }
        public MakeInstance( byte b )
        {
            message = "byte:" + b;
        }
        public MakeInstance( short b )
        {
            message = "short:" + b;
        }
        public MakeInstance( int b )
        {
            message = "int:" + b;
        }
        public MakeInstance( long b )
        {
            message = "long:" + b;
        }
    }

    private String makeInstanceScript( String ctorTypes, String ctorArgs )
    {
        return String.format(
                """
                (
                  (make-instance
                    "de.michab.scream.binding.SchemeObjectTest$MakeInstance:%s"
                    %s)
                  message
                )
                """,
                ctorTypes,
                ctorArgs );
    }

    @Test
    public void make_instance() throws Exception
    {
        var t = makeTester();

        t.expectFco(
                makeInstanceScript(
                        StringUtil.EMPTY_STRING,
                        StringUtil.EMPTY_STRING ),
                str( StringUtil.EMPTY_STRING ) );

        t.expectFco(
                makeInstanceScript(
                        "java.lang.String",
                        "\"x\""),
                str( "String:x" ) );

        //
        // float / Float
        //
        t.expectFco(
                makeInstanceScript(
                        "float",
                        "1.0"),
                str( "float:1.0" ) );
        t.expectFco(
                makeInstanceScript(
                        "java.lang.Float",
                        "1.0"),
                str( "Float:1.0" ) );

        //
        // double
        //
        t.expectFco(
                makeInstanceScript(
                    "double",
                    "1.0"),
                str( "double:1.0" ) );

        t.expectFco(
                makeInstanceScript( "float,double",
                "1.0 2.0"),
                str( "float:1.0:double:2.0" ) );

        t.expectFco(
                makeInstanceScript(
                        "double,float",
                        "1.0 2.0" ),
                str( "double:1.0:float:2.0" ) );

        //
        // boolean
        //
        t.expectFco(
                makeInstanceScript(
                        "boolean",
                        "#t" ),
                str( "boolean:true" ) );
        t.expectFco(
                makeInstanceScript(
                        "boolean",
                        "313" ),
                str( "boolean:true" ) );
        t.expectFco(
                makeInstanceScript(
                        "boolean",
                        "3.14159265" ),
                str( "boolean:true" ) );
        t.expectFco(
                makeInstanceScript(
                        "boolean",
                        "'()" ),
                str( "boolean:true" ) );

        t.expectFco(
                makeInstanceScript(
                        "boolean",
                        "#f" ),
                str( "boolean:false" ) );

        //
        // byte
        //
        t.expectFco(
                makeInstanceScript(
                        "byte",
                        "10" ),
                str( "byte:10" ) );
        // overflow
        t.expectFco(
                makeInstanceScript(
                        "byte",
                        "255" ),
                str( "byte:-1" ) );
        // overflow
        t.expectFco(
                makeInstanceScript(
                        "byte",
                        "256" ),
                str( "byte:0" ) );
        // overflow
        t.expectFco(
                makeInstanceScript(
                        "byte",
                        "257" ),
                str( "byte:1" ) );

        //
        // short
        //
        t.expectFco(
                makeInstanceScript(
                        "short",
                        "10" ),
                str( "short:10" ) );
        t.expectFco(
                makeInstanceScript(
                        "short",
                        "-10" ),
                str( "short:-10" ) );

        //
        // int
        //
        t.expectFco(
                makeInstanceScript(
                        "int",
                        "-10" ),
                str( "int:-10" ) );

        //
        // long
        //
        t.expectFco(
                makeInstanceScript(
                        "long",
                        "-10" ),
                str( "long:-10" ) );

        t.expectError(
                """
                (
                  (make-instance
                    "de.michab.scream.binding.SchemeObjectTest$MakeInstance:float,double"
                    1.0)
                  message
                )
                """,
                Code.WRONG_NUMBER_OF_ARGUMENTS );

        t.expectError(
                """
                (make-instance "MakeInstance:")
                """,
                Code.CLASS_NOT_FOUND );
    }

    @Test
    public void splitAt()
    {
        {
            var result = SchemeObject.splitAt( "a:b", ":" );
            assertEquals( 2, result.size() );
            assertEquals( "a", result.get( 0 ) );
            assertEquals( "b", result.get( 1 ) );
        }
        {
            var result = SchemeObject.splitAt( "a:", ":" );
            assertEquals( 2, result.size() );
            assertEquals( "a", result.get( 0 ) );
            assertEquals( StringUtil.EMPTY_STRING, result.get( 1 ) );
        }
        {
            var result = SchemeObject.splitAt( ":", ":" );
            assertEquals( 2, result.size() );
            assertEquals( StringUtil.EMPTY_STRING, result.get( 0 ) );
            assertEquals( StringUtil.EMPTY_STRING, result.get( 1 ) );
        }
        {
            var result = SchemeObject.splitAt( "::", ":" );
            assertEquals( 3, result.size() );
            assertEquals( StringUtil.EMPTY_STRING, result.get( 0 ) );
            assertEquals( StringUtil.EMPTY_STRING, result.get( 1 ) );
            assertEquals( StringUtil.EMPTY_STRING, result.get( 2 ) );
        }
        {
            var result = SchemeObject.splitAt( "aa:bb:cc", ":" );
            assertEquals( 3, result.size() );
            assertEquals( "aa", result.get( 0 ) );
            assertEquals( "bb", result.get( 1 ) );
            assertEquals( "cc", result.get( 2 ) );
        }
    }
}


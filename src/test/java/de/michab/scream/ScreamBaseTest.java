/*
 * Scream @ https://github.com/urschleim/scream
 *
 * Copyright © 1998-2024 Michael G. Binz
 */
package de.michab.scream;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.fail;

import java.io.File;
import java.io.IOException;
import java.util.function.Function;

import org.junit.jupiter.api.Test;
import org.smack.util.FunctionalUtil.ConsumerTX;

import de.michab.scream.RuntimeX.Code;
import de.michab.scream.fcos.Bool;
import de.michab.scream.fcos.Cons;
import de.michab.scream.fcos.FirstClassObject;
import de.michab.scream.fcos.Int;
import de.michab.scream.fcos.Port;
import de.michab.scream.fcos.Real;
import de.michab.scream.fcos.SchemeString;
import de.michab.scream.fcos.Symbol;
import de.michab.scream.frontend.SchemeParser;
import de.michab.scream.util.Continuation;
import de.michab.scream.util.Scut;

/**
 * scream test support.
 *
 * Note that the tests testing the test support are also implemented here.
 *
 * @author micbinz
 */
public class ScreamBaseTest
{
    public final static Int i1 = i( 1 );
    public final static Int i2 = i( 2 );
    public final static Int i3 = i( 3 );
    public final static Int i4 = i( 4 );
    public final static Int i313 = i( 313 );

    public final static Symbol s1 = s( "one" );
    public final static Symbol s2 = s( "two" );
    public final static Symbol s3 = s( "three" );
    public final static Symbol s4 = s( "four" );
    public final static Symbol s313 = s( "threethirteen" );

    public final static Bool bTrue = Bool.T;
    public final static Bool bFalse = Bool.F;

    public static <S extends FirstClassObject,J>
    void toJava_( Class<S> sc, Class<J> jc, J testObject, Function<J, S> factory) throws RuntimeX
    {
        S so = factory.apply( testObject );
        assertNotNull( so );
        assertInstanceOf( sc, so );
        var j = so.toJava();
        assertNotNull( j );
        assertInstanceOf( jc, j );
        assertEquals( testObject, j );
    }

    /**
     * Parses the passed Scheme expression and returns the expected type.
     *
     * @param <S> The expected result type.
     * @param scheme A Scheme expression.
     * @param cl The expected type.
     * @return The read FirstClassObject as type S.
     * @throws RuntimeX If evaluation of the expression failed.
     * @throws ClassCastException if the expression does not evaluate to S.
     */
    static public <S extends FirstClassObject> S parse(
            String scheme,
            Class<S> cl )
        throws RuntimeX
    {
        return cl.cast( parse( scheme ) );
    }

    /**
     * Parses the passed Scheme expression.
     *
     * @param scheme A Scheme expression.
     * @return The evaluation result.
     * @throws RuntimeX If evaluation of the expression failed.
     */
    static public FirstClassObject parse(
            String scheme )
        throws RuntimeX
    {
        var parser =
                new SchemeParser( scheme );
        var fco =
                parser.getExpression();
        assertEquals(
                Port.EOF,
                parser.getExpression() );
        return fco;
    }

    /**
     * Evaluate a script and check whether the result is
     * (equal? ...) to a given value.
     *
     * @param result The evaluator to use.
     * @param script The script to execute.
     * @param expected The expected result.  This may be Cons.NIL.
     * @return The script engine used in the test.
     */
    protected ScreamEvaluator expectFco(
            ScreamEvaluator result,
            String script,
            FirstClassObject expected )
                    throws RuntimeX
    {
        assertEqualq(
                expected,
                result.evalFco( script ) );
        return result;
    }

    /**
     * Evaluate a script and check whether the result is
     * (equal? ...) to a given value.
     *
     * @param script The script to execute.
     * @param expected The expected result.  This may be Cons.NIL.
     * @return The script engine created and used in the test.
     */
    protected ScreamEvaluator expectFco(
            String script,
            FirstClassObject expected )
                    throws RuntimeX
    {
        return expectFco(
                scriptEngine(),
                script,
                expected );
    }

    /**
     * Evaluate a script and check whether the result is
     * (equal? ...) to a given value.
     *
     * @param script The script to execute.
     * @param expected The expected result as Scheme definition.
     * @return The script engine used in the test.
     */
    protected ScreamEvaluator expectFco(
            String script,
            String expected )
                    throws RuntimeX
    {
        return expectFco( script, parse(expected) );
    }

    /**
     *
     */
    public static interface Tester {
        /**
         * Execute the passed script and return the result without
         * validation.
         *
         * @param script The script to execute.
         * @return The execution result.
         * @throws RuntimeX In case of an error.
         */
        FirstClassObject execute( String script ) throws RuntimeX;
        <R extends FirstClassObject> R execute( String script, Class<R> cl ) throws RuntimeX;
        Tester expectFco( String script, String result ) throws RuntimeX;
        Tester expectFco( String script, FirstClassObject result ) throws RuntimeX;
        RuntimeX expectError( String script, Code result );
    }

    /**
     * Creates a stateful tester.
     */
    protected Tester makeTester()
    {
        return new Tester() {

            ScreamEvaluator se = scriptEngine();

            @Override
            public Tester expectFco( String script, String result ) throws RuntimeX

            {
                ScreamBaseTest.this.expectFco( se, script, parse( result ) );
                return this;
            }

            @Override
            public Tester expectFco( String script, FirstClassObject result ) throws RuntimeX
            {
                ScreamBaseTest.this.expectFco( se, script, result  );
                return this;
            }

            @Override
            public RuntimeX expectError( String script, Code result )
            {
                return ScreamBaseTest.this.expectError( se, script, result );
            }

            @Override
            public FirstClassObject execute( String script ) throws RuntimeX
            {
                return se.evalFco( script );
            }

            @Override
            public <R extends FirstClassObject> R execute( String script,
                    Class<R> cl ) throws RuntimeX
            {
                return Scut.as(
                        cl,
                        execute( script ) );
            }
        };
    }

    /**
     * Evaluate the passed expression and expect an error.
     *
     * @param expression The expression to evaluate.
     * @param expected The expected error code.
     * @return The exception that was thrown.
     */
    protected RuntimeX expectError(
            String expression,
            Code expected )
    {
        return expectError(
                scriptEngine(), expression, expected );
    }

    protected RuntimeX expectError(
            ScreamEvaluator se,
            String expression,
            Code expected )
    {
        try
        {
            se.evalFco( expression );
            fail();
        }
        catch ( RuntimeX rx )
        {
            assertEquals( expected, rx.getCode() );
            return rx;
        }
        throw new InternalError( "Unexpected." );
    }

    protected void assertEqualq( FirstClassObject expected, FirstClassObject actual )
    {
        if ( FirstClassObject.equal( expected, actual ) )
            return;

        var msg = String.format(
                "Expected '%s', got '%s'.",
                FirstClassObject.toString( expected ),
                FirstClassObject.toString( actual ) );
        fail( msg );
    }

    /**
     * @return A newly created script engine.
     */
    protected ScreamEvaluator scriptEngine()
    {
        return (ScreamEvaluator)new Scream().getScriptEngine();
    }

    public static Symbol s( String name )
    {
        return Symbol.createObject( name );
    }
    public static SchemeString str( String name )
    {
        return SchemeString.make( name );
    }
    public static Int i( long v )
    {
        return Int.createObject( v );
    }
    protected static Cons c( FirstClassObject ... fcos )
    {
        return Cons.create( fcos );
    }

    /**
     * Create a scream double.
     *
     * @param v The value of the double.
     * @return The requested value.
     */
    protected static Real d( double v )
    {
        return Real.createObject( v );
    }

    protected File tmpFile( Class<?> caller ) throws IOException
    {
        return File.createTempFile( caller.getSimpleName(), ".tmp" );
    }

    /**
     * Creates a file and passes it to the received lambda.
     * After execution of the operation, the passed file is deleted.
     *
     * @param operation The lambda to execute.
     * @throws Exception
     */
    protected void withFile( ConsumerTX<File,Exception> operation )
        throws Exception
    {
        var f = tmpFile( getClass() );

        try
        {
           operation.accept( f );
        }
        finally {
            f.delete();
        }
    }

    protected Continuation<FirstClassObject> cont()
    {
        return new Continuation<FirstClassObject>();
    }


    @Test
    public void makeTester_test() throws Exception
    {
        var t = makeTester();

        // Uses state. 'a is defined in step 1.
        t.execute( "(define a 1)" );
        // 'a referenced in step 2.
        t.expectFco( "a", "1" );
    }
}

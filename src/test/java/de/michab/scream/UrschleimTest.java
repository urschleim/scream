/*
 * Scream @ https://github.com/michab/dev_smack
 *
 * Copyright © 2022 Michael G. Binz
 */
package de.michab.scream;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;

import org.junit.jupiter.api.Test;

import de.michab.scream.frontend.SchemeParser;
import urschleim.Continuation;
import urschleim.Holder;

public class UrschleimTest
{
    @Test
    public void typeIntegerTest() throws Exception
    {
        Holder<RuntimeX> error =
                new Holder<RuntimeX>( null );

        var i = SchemeInteger.createObject( 313 );

        Holder<FirstClassObject> r =
                new Holder<FirstClassObject>( Cons.NIL );

        Continuation c = new Continuation( s -> error.set( s ) );

        c.trampoline(
                i.evaluate( null,
                        Continuation.endCall( s -> r.set( s ) ) ));

        assertEquals( "313", r.get().toString() );
        assertNull( error.get() );
    }

    @Test
    public void typeSymbolTest() throws Exception
    {
        var symbol = Symbol.createObject( "car" );
        var env = new Environment();
        env.set( symbol, SchemeInteger.createObject( 313 ) );

        Holder<FirstClassObject> r =
                new Holder<FirstClassObject>( Cons.NIL );

        Holder<ScreamException> error =
                new Holder<>( null );

        Continuation.trampoline(
                symbol.evaluate( env,
                        Continuation.endCall( s -> r.set( s ) ) ),
                s -> error.set( s ));

        assertEquals( "313", r.get().toString() );
        assertNull( error.get() );
    }

    @Test
    public void typeSymbolErrorTest() throws Exception
    {
        var symbol = Symbol.createObject( "car" );
        var env = new Environment();

        Holder<FirstClassObject> r =
                new Holder<FirstClassObject>( Cons.NIL );
        Holder<ScreamException> error =
                new Holder<>( null );

        Continuation.trampoline(
                symbol.evaluate( env,
                        Continuation.endCall( s -> r.set( s ) ) ),
                s -> error.set(s));

        assertNull(
                r.get() );
        assertNotNull(
                error.get() );
        assertEquals(
                ScreamException.Code.SYMBOL_NOT_DEFINED,
                error.get().getCode() );
    }

    @Test
    public void operationTest() throws Exception
    {
        FirstClassObject add313 =
                new SchemeParser( "(+ 300 13)" ).getExpression();
        assertInstanceOf( Cons.class, add313 );

        SchemeInterpreter2 si = new SchemeInterpreter2();
        var se = (SchemeEvaluator2)si.getScriptEngine();

        var env = se.getInteraction();

        Holder<FirstClassObject> r =
                new Holder<FirstClassObject>( Cons.NIL );
        Holder<ScreamException> error =
                new Holder<>( null );

        Continuation.trampoline(
                add313.evaluate( env,
                        Continuation.endCall( s -> r.set( s ) ) ),
                s -> error.set( s ) );

        assertNotNull(
                r.get() );
        assertInstanceOf(
                SchemeInteger.class,
                r.get() );
        assertEquals(
                313,
                ((SchemeInteger)r.get()).asLong() );
        assertNull(
                error.get() );
    }



    @Test
    public void listEvalTest() throws Exception
    {
        var i1 = SchemeInteger.createObject( 1 );
        var i2 = SchemeInteger.createObject( 2 );
        var i3 = SchemeInteger.createObject( 3 );
        var i4 = SchemeInteger.createObject( 4 );

        Environment env = new Environment();
        env.set(
                Symbol.createObject( "one" ),
                i1 );
        env.set(
                Symbol.createObject( "two" ),
                i2 );
        env.set(
                Symbol.createObject( "three" ),
                i3 );
        env.set(
                Symbol.createObject( "four" ),
                i4 );

        FirstClassObject list1234c =
                new SchemeParser( "(one two three four)" ).getExpression();
        assertInstanceOf( Cons.class, list1234c );
        var array1234 = ((Cons)list1234c).asArray();

        Holder<FirstClassObject[]> r =
                new Holder<>( null );
        Holder<ScreamException> error =
                new Holder<>( null );

        Continuation.trampoline(
                Continuation.listEval(
                        env,
                        array1234,
                        Continuation.endCall( s -> r.set( s ) ) ),
                s -> error.set( s ) );

        assertNotNull(
                r.get() );
        assertEquals(
                i1,
                r.get()[0] );
        assertEquals(
                i2,
                r.get()[1] );
        assertEquals(
                i3,
                r.get()[2] );
        assertEquals(
                i4,
                r.get()[3] );
        assertNull(
                error.get() );
    }
}

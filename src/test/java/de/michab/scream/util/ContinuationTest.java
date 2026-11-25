/*
 * Scream @ https://github.com/urschleim/scream
 *
 * Copyright © 1998-2023 Michael G. Binz
 */
package de.michab.scream.util;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

import org.junit.jupiter.api.Test;

import de.michab.scream.Raise;
import de.michab.scream.RuntimeX;
import de.michab.scream.RuntimeX.Code;
import de.michab.scream.ScreamBaseTest;
import de.michab.scream.fcos.FirstClassObject;
import de.michab.scream.fcos.Int;
import de.michab.scream.util.Continuation.Cont;
import de.michab.scream.util.Continuation.Thunk;

public class ContinuationTest extends ScreamBaseTest
{
    private Thunk add( int a, int b, Cont<Integer> c )
    {
        return () -> c.accept( a + b );
    }

    private Thunk addd( int a, int b, Cont<Integer> c )
    {
        return () -> () -> c.accept( a + b );
    }

    private Thunk addx( int a, int b, Cont<Integer> c )
    {
        return () -> {
            throw new ArithmeticException( "addx" );
        };
    }

    @Test
    public void basic() throws Exception
    {
        Continuation<Integer> continuation =
                new Continuation<>();

        continuation.thunkCount(0);

        int result = continuation.toStack(
                cont -> add(3,4,cont) );

        assertEquals( 1, continuation.thunkCount() );
        assertEquals( 7, result );
    }

    @Test
    public void basic2() throws Exception
    {
        Continuation<Integer> continuation =
                new Continuation<>();

        continuation.thunkCount(0);

        int result = continuation.toStack(
                cont-> addd(3,4,cont) );

        assertEquals( 2, continuation.thunkCount() );
        assertEquals( 7, result );
    }

    @Test
    public void testSuccess() throws Exception
    {
        Continuation<Integer> continuation =
                new Continuation<>();

        int result = continuation.toStack(
                cont-> add(3,4,cont) );

        assertEquals( 7, result );
    }

    @Test
    public void testException()
    {
        Continuation<Integer> continuation =
                new Continuation<>();

        assertThrows(
                ArithmeticException.class,
                () -> continuation.toStack(
                    cont-> addx(3,4,cont) ) );
    }

    private Thunk addFco( Int si1, Int si2, Cont<FirstClassObject> cont )
    {
        return () -> cont.accept( si1.add( si2 ) );
    }

    @Test
    public void testFcoFunc() throws Exception
    {
        Continuation<FirstClassObject> cont = new Continuation<>();

        var fco = cont.toStack( continuation -> addFco( i1, i2, continuation ) );

        assertEqualq( i3, fco );
    }

    private Thunk throwRtx( Int si1, Int si2, Cont<FirstClassObject> cont )
            throws RuntimeX
    {
        return () -> { throw Raise.mDivisionByZero(); };
    }

    @Test
    public void testFcoException() throws Exception
    {
        Continuation<FirstClassObject> cont = new Continuation<>();

        Continuation.ToStackOp<FirstClassObject> tsfco = c -> throwRtx( i1, i2, c );

        var rx = assertThrows(
                RuntimeX.class,
                () -> cont.toStack( tsfco ) );
        assertEquals( Code.DIVISION_BY_ZERO, rx.getCode() );
    }
}

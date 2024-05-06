/*
 * Scream @ https://github.com/urschleim/scream
 *
 * Copyright © 2024 Michael G. Binz
 */
package de.michab.scream.language;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;

import org.junit.jupiter.api.Test;

import de.michab.scream.RuntimeX;
import de.michab.scream.RuntimeX.Code;
import de.michab.scream.ScreamBaseTest;
import de.michab.scream.fcos.Cons;
import de.michab.scream.fcos.Symbol;
import de.michab.scream.util.ConsToString;
import de.michab.scream.util.Scut;

public class R7rs_2_4_Datum_labels_Test extends ScreamBaseTest
{
    private void addStepperEngine( Tester t ) throws RuntimeX
    {
        t.execute(
"""
(define (stepper list)
  (let ((current list))
     (lambda ()
       (let ((result (car current)))
         (set! current (cdr current))
         result))))
"""
         );
    }

    /**
     * r7rs 2.4 p9
     */
    @Test
    public void r7rs_example() throws Exception
    {
        var t = makeTester();

        t.expectFco(
"""
                (scream:circular?
                  '#0=(a b c . #0#)
                )
""",
                bTrue );
        t.expectError(
                "#0= #0#",
                Code.SYNTAX_ERROR );
        t.expectError(
                "#0=#0#",
                Code.SYNTAX_ERROR );
        t.expectError(
                "#313=#0#",
                Code.SYNTAX_ERROR );
    }

    @Test
    public void iterate_simple() throws Exception
    {
        var t = makeTester();

        addStepperEngine( t );

        t.execute(
"""
                (define circular
                  '#0=(a b c . #0#)
                )
"""
                );
        t.execute(
"""
                (define nx (stepper circular))"""
                );
        t.expectFco( "(nx)", s("a") );
        t.expectFco( "(nx)", s("b") );
        t.expectFco( "(nx)", s("c") );
        t.expectFco( "(nx)", s("a") );
    }

    @Test
    public void iterate_simple_1() throws Exception
    {
        var t = makeTester();

        addStepperEngine( t );

        t.execute(
"""
                (define circular
                  '(a . #0=(b c . #0#))
                )
"""
                );
        t.execute(
"""
                (define nx (stepper circular))"""
                );
        t.expectFco( "(nx)", s("a") );
        t.expectFco( "(nx)", s("b") );
        t.expectFco( "(nx)", s("c") );
        t.expectFco( "(nx)", s("b") );
    }

    /**
     * Test from https://docs.scheme.org/surveys/datum-labels/
     */
    @Test
    public void schemeOrgSurveysDatumLabels() throws Exception
    {
        expectFco(
                "(let ((x '(#1=(a b) #1#))) (eq? (car x) (cadr x)))",
                bTrue );
    }


    private void prefixEquals( String prefix, Cons cons ) throws RuntimeX
    {
        Symbol skip = Symbol.createObject( "*" );

        var prefixCons = Scut.as( Cons.class, parse( prefix ) );

        for ( var fco : prefixCons )
        {
            if ( ! skip.eq( fco ) )
                assertEqualq( fco, cons.getCar() );

            cons = Scut.as( Cons.class, cons.getCdr() );
        }
    }

    @Test
    public void toDatumLabelString1() throws Exception
    {
        var cons = Scut.as(
                Cons.class,
                parse( "()" ) );

        var cts = new ConsToString( cons );

        System.out.println( cts );
    }

    @Test
    public void plainProper() throws Exception
    {
        var expected = "(m i c b i n z)";

        var cons = Scut.as(
                Cons.class,
                parse( expected ) );

        var cts = new ConsToString( cons ).toString();

        assertEquals( expected, cts );
    }

    @Test
    public void plainProperNested() throws Exception
    {
        var expected = "((m 1) (i 2) (c 3) (b 4) (i 5) (n 6) (z 7))";

        var cons = Scut.as(
                Cons.class,
                parse( expected ) );

        var cts = new ConsToString( cons ).toString();

        assertEquals( expected, cts );
    }

    @Test
    public void plainImproper() throws Exception
    {
        var expected = "(m i c b i n z . 313)";

        var cons = Scut.as(
                Cons.class,
                parse( expected ) );

        var cts = new ConsToString( cons ).toString();

        assertEquals( expected, cts );
    }

    @Test
    public void toDatumLabelString2() throws Exception
    {
        var cons = Scut.as(
                Cons.class,
                parse( "(m i c b i n z)" ) );

        var cts = new ConsToString( cons ).toString();

        prefixEquals(
                "(m i c b i n z)",
                Scut.as(
                        Cons.class,
                        parse( cts ) ) );
    }

    @Test
    public void toDatumLabelString209() throws Exception
    {
        var cons = Scut.as(
                Cons.class,
                parse( "#0=(a b c . #0#)" ) );

        var cts = new ConsToString( cons ).toString();

        prefixEquals(
                "(a b c a)",
                Scut.as(
                        Cons.class,
                        parse( cts ) ) );
    }

    @Test
    public void toDatumLabelString234() throws Exception
    {
        var cons = Scut.as(
                Cons.class,
                parse( "(a . #0=(b c . #0#))" ) );

        var cts = new ConsToString( cons ).toString();

        prefixEquals(
                "(a b c b c)",
                Scut.as(
                        Cons.class,
                        parse( cts ) ) );
    }

    @Test
    public void toDatumLabelString3() throws Exception
    {
        var cons = assertInstanceOf(
                Cons.class,
                parse( "(1 #2=(11 . #3=(12 . #2#)) 2 . #3#)" ) );

        var cts = new ConsToString( cons ).toString();

        prefixEquals(
                "(1 * 2 12 11 12 11)",
                Scut.as(
                        Cons.class,
                        parse( cts ) ) );
    }

    private void testLabelAdjustment( String in, String out ) throws Exception
    {
        var cons = assertInstanceOf(
                Cons.class,
                parse( in ) );

        assertEquals(
                out,
                new ConsToString( cons ).toString() );
    }

    @Test
    public void labelAdjustment0() throws Exception
    {
        testLabelAdjustment(
                // Labels 2 3 ...
                "(1 #2=(11 . #3=(12 . #2#)) 2 . #3#)",
                // ... 0 1
                "(1 #0=(11 . #1=(12 . #0#)) 2 . #1#)" );
    }

    @Test
    public void labelAdjustment1() throws Exception
    {
        testLabelAdjustment(
                "(1 #313=(11 . #0=(12 . #313#)) 2 . #0#)",
                "(1 #0=(11 . #1=(12 . #0#)) 2 . #1#)" );
    }

    @Test
    public void labelAdjustment2() throws Exception
    {
        testLabelAdjustment(
                "(a . #0=(b c . #0#))",
                "(a . #0=(b c . #0#))" );
    }

    @Test
    public void labelAdjustment3() throws Exception
    {
        testLabelAdjustment(
                "(a . #9=(b c . #9#))",
                "(a . #0=(b c . #0#))" );
    }

    @Test
    public void labelAdjustment4() throws Exception
    {
        testLabelAdjustment(
                "#1=(a b c . #1#)",
                "#0=(a b c . #0#)" );
    }
}

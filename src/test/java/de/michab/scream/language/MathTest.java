/*
 * Scream @ https://github.com/michab/dev_smack
 *
 * Copyright © 1998-2022 Michael G. Binz
 */

package de.michab.scream.language;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.fail;

import java.util.logging.Level;
import java.util.logging.Logger;

import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;

import de.michab.scream.Cons;
import de.michab.scream.FirstClassObject;
import de.michab.scream.SchemeBoolean;
import de.michab.scream.ScreamBaseTest;
import de.michab.scream.ScreamEvaluator;
import de.michab.scream.ScreamException;
import de.michab.scream.Symbol;
import de.michab.scream.pops.Continuation;
import urschleim.Holder;

public class MathTest extends ScreamBaseTest
{
    private static final Logger LOG = Logger.getLogger(
            MathTest.class.getName() );

    @Test
    public void mathEquals_t() throws Exception
    {
        ScreamEvaluator se = scriptEngine();

        var result = se.evalFco(
                """
                (= 313 313)
                """ );
        assertEquals( SchemeBoolean.T, result );
    }
    @Test
    public void mathEquals_f() throws Exception
    {
        ScreamEvaluator se = scriptEngine();

        var result = se.evalFco(
                """
                (= 121 313)
                """ );
        assertEquals( SchemeBoolean.F, result );
    }

    @Test
    @Disabled
    public void _equalsCompile() throws Exception
    {
        String code = "(= 121 121)";

        ScreamEvaluator se = scriptEngine();

        var result = se.evalFco(
                code );
        assertEquals( SchemeBoolean.T, result );

        FirstClassObject opCall = parse(
                code );
        assertInstanceOf( Cons.class, opCall );

        var env = se.getInteraction();

        Holder<FirstClassObject> r =
                new Holder<FirstClassObject>( Cons.NIL );
        Holder<ScreamException> error =
                new Holder<>( null );

        Continuation.trampoline(
                opCall.evaluate( env,
                        Continuation.endCall( s -> r.set( s ) ) ),
                s -> error.set( s ) );

        if ( error.get() != null )
        {
            LOG.log( Level.SEVERE, error.get().getMessage(), error.get() );
            fail();
        }

        assertNotNull(
                r.get() );
        assertInstanceOf(
                Symbol.class,
                r.get() );
        assertEquals(
                Symbol.createObject( "not-defined-yet" ),
                r.get() );
    }


}

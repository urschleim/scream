/*
 * Scream @ https://github.com/urschleim/scream
 *
 * Copyright © 1998-2023 Michael G. Binz
 */
package de.michab.scream.pops;

import org.smack.util.TimeProbe;

import de.michab.scream.RuntimeX;
import de.michab.scream.fcos.Cons;
import de.michab.scream.fcos.Environment;
import de.michab.scream.fcos.FirstClassObject;
import de.michab.scream.fcos.Int;
import de.michab.scream.fcos.Syntax;
import de.michab.scream.util.Continuation.Cont;
import de.michab.scream.util.Continuation.Thunk;

/**
 * {@code (%time expression)}
 * <p>
 * Returns a pair where the car part holds the time that {@code expression}
 * needed to execute and the cdr holds the result of {@code expression}.
 */
public class SyntaxTime extends Syntax
{
    private SyntaxTime()
    {
        super("%time" );
    }

    /**
     * Processes the result of the timing request.
     *
     * @param tp The timer.
     * @param evalResult The result of expression evaluation.
     * @param c The final continuation receiving the pair.
     * @return The thunk.
     * @throws RuntimeX
     */
    private Thunk finish(
            TimeProbe tp,
            FirstClassObject evalResult,
            Cont<FirstClassObject> c ) throws RuntimeX
    {
        var result = new Cons(
                Int.createObject( tp.stop().duration() ),
                evalResult );

        return c.accept( result );
    }

    private Thunk _thunked_finish(
            TimeProbe tp,
            FirstClassObject evalResult,
            Cont<FirstClassObject> c )
    {
        return () -> finish( tp, evalResult, c );
    }

    @Override
    protected Thunk _executeImpl( Environment e, Cons args,
            Cont<FirstClassObject> c ) throws RuntimeX
    {
        checkArgumentCount( 1, args );

        var expression = args.getCar();

        return () -> {
            TimeProbe tp =
                    new TimeProbe( getName().toString() ).start();
            return Primitives._eval(
                    e,
                    expression,
                    fco -> _thunked_finish( tp, fco, c ) );
        };
    }

    /**
     * Base operations setup.
     *
     * @param tle A reference to the environment to be extended.
     * @return The extended environment.
     */
    public static Environment extendNullEnvironment( Environment tle )
            throws RuntimeX
    {
        tle.setPrimitive( new SyntaxTime() );

        return tle;
    }
}

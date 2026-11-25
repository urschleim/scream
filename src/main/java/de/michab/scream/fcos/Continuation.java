/*
 * Scream @ https://github.com/urschleim/scream
 *
 * Copyright © 2023 Michael G. Binz
 */
package de.michab.scream.fcos;

import de.michab.scream.RuntimeX;
import de.michab.scream.pops.Primitives;
import de.michab.scream.pops.REGS;
import de.michab.scream.util.Continuation.Cont;
import de.michab.scream.util.Continuation.Thunk;
import de.michab.scream.util.Scut;

/**
 * The continuation-type that is passed as an exit procedure into the
 * procedure received by {@code call-with-current-continuation}.
 *
 * @author micbinz
 */
public class Continuation extends Procedure implements Cont<FirstClassObject>
{
    private final Cont<FirstClassObject> _cont;

    /**
     * Create an instance.
     *
     * @param continuation The continuation to execute.
     */
    public Continuation(
            Cont<FirstClassObject> continuation,
            Environment closure )
    {
        super( "callcc", closure );

        _cont = continuation;
    }
    public Continuation(
            Cont<FirstClassObject> continuation)
    {
        super( "callcc", REGS.CENV().copy() );

        _cont = continuation;
    }

    /**
     * (call-with-current-continuation ...
     */
    static private Procedure callccProc( Environment e )
    {
        return new Procedure( "call-with-current-continuation", e )
        {
            @Override
            protected Thunk _executeImpl(
                    Environment e,
                    Cons args,
                    Cont<FirstClassObject> c )
                            throws RuntimeX
            {
                checkArgumentCount( 1, args );

                var proc = Scut.as(
                        Procedure.class,
                        args.getCar() );

                return proc.execute(
                        e,
                        Cons.create(
                                new Continuation( c ) ),
                        c );
            }
        };
    }

    /**
     * (call-with-values ...
     */
    static private Procedure callWithValuesProc( Environment e )
    {
        return new Procedure( "call-with-values", e )
        {

            @Override
            protected Thunk _executeImpl(
                    Environment e,
                    Cons args,
                    Cont<FirstClassObject> c )
                            throws RuntimeX
            {

                checkArgumentCount( 2, args );

                var producer = Scut.as(
                        Procedure.class,
                        args.listRef(0) );
                var consumer = Scut.as(
                        Procedure.class,
                        args.listRef(1) );

                Cont<Cons> branch = values ->
                {
                    return consumer.execute(
                            e,
                            values,
                            c );
                };

                return producer.execute(
                        e,
                        Cons.NIL,
                        values -> Primitives._cast(
                                Cons.class,
                                values,
                                branch ) );
            }
        };
    }

    @Override
    protected Thunk _executeImpl( Environment e, Cons args,
            Cont<FirstClassObject> c ) throws RuntimeX
    {
        var values = args.length() == 1 ?
                args.getCar() :
                args;

        // Recreates the original environment.
        REGS.CENV( closure() );

        return _cont.accept( values );
    }

    /**
     * Operations setup.
     *
     * @param tle The toplevel-environment to extend.
     * @return The extended environment.
     * @throws RuntimeX
     */
    public static Environment extendTopLevelEnvironment( Environment tle )
            throws RuntimeX
    {
        tle.setPrimitive( callWithValuesProc( tle ) );
        var ccc = callccProc( tle );
        tle.setPrimitive( ccc );
        tle.define( Symbol.createObject( "call/cc" ), ccc );

        return tle;
    }

    @Override
    public Thunk accept( FirstClassObject result ) throws RuntimeX
    {
        return _cont.accept( result );
    }
}

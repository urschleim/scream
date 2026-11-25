/*
 * Scream @ https://github.com/urschleim/scream
 *
 * Copyright © 1998-2022 Michael G. Binz
 */
package de.michab.scream.fcos;

import de.michab.scream.RuntimeX;
import de.michab.scream.pops.Primitives;
import de.michab.scream.util.Continuation.Cont;
import de.michab.scream.util.Continuation.Thunk;

/**
 * Represents a Scheme procedure.
 */
public class Procedure
    extends Operation
{
    /**
     * The name of the type as used by error reporting.
     *
     * @see FirstClassObject#typename()
     */
    public static final String TYPE_NAME = "procedure";

    /**
     * This procedure's effective environment.
     *
     * @label effective environment
     */
    private final Environment _closure;

    /**
     * A constructor for Java-defined procedures.
     *
     * @param name The symbolic name for the new procedure.
     */
    protected Procedure( String name, Environment closure )
    {
        super( Symbol.createObject( name ) );

        _closure = closure;
    }

    /**
     * Constructor used to create Scheme-defined procedures.
     *
     * @param e The new procedure's closure.
     * @param args The list of formal arguments.
     * @param body The body of the new procedure.
     * @throws RuntimeX In case an error occurred.
     */
    public Procedure(
            Environment e,
            FirstClassObject args,
            Cons body  )
                    throws RuntimeX
    {
        super( args, body );

        _closure = e;
    }

    /**
     * Evaluates the arguments in the received environment and executes
     * the Procedure.
     *
     * @param e The environment used for argument evaluation.
     * @param args The arguments for the execution.
     * @param c Receives the result.
     * @return A thunk.
     * @throws RuntimeX
     */
    @Override
    public final Thunk execute( Environment e, Cons args, Cont<FirstClassObject> c )
        throws RuntimeX
    {
        return Primitives._evalCons(
                e,
                args,
                evaluated -> apply( evaluated, c ) );
    }

    /**
     * Executes this Procedure with the passed arguments.
     * The arguments are expected to be already evaluated.
     * No environment needed since the Procedure is executed
     * in its closure.
     * <p>
     * Java-defined procedures implement their body by overriding
     * {@code _executeImpl(Environment, Cons, Cont)} (see {@link Operation}),
     * not this operation.
     *
     * @param args The already evaluated arguments.
     * @param c The result.
     * @return A thunk.
     * @throws RuntimeX
     */
    public Thunk apply( Cons args, Cont<FirstClassObject> c  )
            throws RuntimeX
    {
        return _executeImpl(
                _closure,
                args,
                c );
    }

    /**
     * @return The Procedure's closure.
     */
    protected Environment closure()
    {
        return _closure;
    }
}

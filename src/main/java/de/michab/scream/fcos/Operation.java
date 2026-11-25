/*
 * Scream @ https://github.com/urschleim/scream
 *
 * Copyright © 1998-2022 Michael G. Binz
 */
package de.michab.scream.fcos;

import java.util.HashSet;
import java.util.Set;

import de.michab.scream.Raise;
import de.michab.scream.RuntimeX;
import de.michab.scream.pops.Primitives;
import de.michab.scream.util.Continuation.Cont;
import de.michab.scream.util.Continuation.Thunk;
import de.michab.scream.util.Scut;

/**
 * Represents an abstract operation.  Is the base class for macros
 * and procedures.
 *
 * @author Michael Binz
 */
public abstract class Operation
extends FirstClassObject
{
    /**
     * The name of the type as used by error reporting.
     *
     * @see FirstClassObject#typename()
     */
    public static final String TYPE_NAME = Operation.class.getSimpleName();

    /**
     * A default name used in the no-argument constructor.
     */
    static final Symbol DEFAULT_NAME =
            Symbol.createObject( "anonymous" );

    /**
     * A name for this Operation.
     */
    private Symbol _name = null;

    /**
     * This operation's body.
     */
    private final Cons _body;

    /**
     * The formal argument list for this operation.
     */
    private Cons _formalArguments = Cons.NIL;

    /**
     * The symbol being bound to the rest of the argument list.  This symbol can
     * be used to access the dynamic arguments of an operation invocation.
     */
    private Symbol _rest = null;

    /**
     * Creates a named operation.  Main purpose is for operation-derived objects
     * implemented in Java.
     *
     * @param name A symbolic name for the new operation.  Used for error
     *             reporting.
     */
    protected Operation( Symbol name )
    {
        _name = name;
        _body = Cons.NIL;
    }
    protected Operation( String name )
    {
        this( Symbol.createObject( name ) );
    }

    /**
     * Used for scheme defined @{code Operation} objects.
     *
     * @param formalArguments The operation's formal arguments as specified in
     *        the source code.
     * @param body The operation's body.
     * @throws RuntimeX In case the definition is syntactically wrong.
     */
    protected Operation( FirstClassObject
            formalArguments,
            Cons body )
                    throws RuntimeX
    {
        _name = DEFAULT_NAME;

        _body = body;

        // If the formal argument list is empty ...
        if ( Cons.NIL == formalArguments )
            // ...we are ready.
            return;

        // If the formal arguments are only a symbol...
        if ( formalArguments instanceof Symbol )
        {
            // ...this means variable number of arguments.
            _rest = (Symbol)formalArguments;
            // Ready.
            return;
        }

        // The formal arguments must be a list.
        if ( ! (formalArguments instanceof Cons) )
            throw Raise.mSyntaxError( formalArguments );

        // Remember the formal argument list.
        Cons fac = _formalArguments =
                Scut.as( Cons.class, formalArguments ).copy();

        // The formal argument list must be a list of unique symbols.
        Set<String> unifier = new HashSet<String>();

        // TODO rethink this loop ... especially the break conditions.
        while ( fac != Cons.NIL )
        {
            // Check the car.
            FirstClassObject car = fac.getCar();

            if ( car instanceof Symbol )
            {
                String symbolName = car.toString();

                // Set.add() returns false if the element is already contained.
                if ( false == unifier.add( symbolName ) )
                    throw Raise.mDuplicateFormal( car );

                // This cell was fine.
            }
            else
                throw Raise.mInvalidFormals(  car );

            // Check the cdr.
            FirstClassObject cdr = fac.getCdr();

            if ( cdr == Cons.NIL )
                break;

            else if ( cdr instanceof Cons )
                // Standard case.
                fac = (Cons)cdr;

            else if ( cdr instanceof Symbol )
            {
                // A 'rest' symbol was defined.
                // Set.add() returns false if the element is already contained.
                if ( false == unifier.add( cdr.toString() ) )
                    throw Raise.mDuplicateFormal( cdr );
                else
                {
                    // Terminate the formal argument list...
                    fac.setCdr( Cons.NIL );
                    // ...and remember the rest symbol.
                    _rest = (Symbol)cdr;
                }
                // Done.  Break out of the loop.
                break;
            }
            else
                throw Raise.mInvalidFormals( cdr );
        }
    }

    /**
     * @return {@code true} if this is a variadic operation.
     */
    public boolean isVariadic()
    {
        return _rest != null;
    }

    public long argumentCount()
            throws RuntimeX
    {
        return _formalArguments == null ?
                0 :
                    _formalArguments.length();
    }

    /**
     * Binds the argument values to their respective names in the
     * passed environment.
     *
     * @param e The environment that receives the bindings.
     * @param argNames The names to bind.
     * @param argValues The values to bind.
     * @param c The continuation receiving the extended environment.
     * @return A thunk.
     * @throws RuntimeX
     */
    private Thunk _bind( Environment e, Cons argNames, Cons argValues, Cont<Environment> c )
            throws RuntimeX
    {
        if ( argNames != Cons.NIL && argValues == Cons.NIL)
            // Cannot happen since we check in _activate.
            throw new InternalError( "NotEnough" );
        if ( argNames == Cons.NIL && argValues == Cons.NIL )
            return c.accept( e );
        if ( argNames == Cons.NIL && isVariadic() )
        {
            e.define( _rest, argValues );
            return c.accept( e );
        }
        if ( argNames == Cons.NIL && ! isVariadic() )
        {
            // Cannot happen since we check in _activate.
            throw new InternalError( "TooMany" );
        }

        Symbol name =
                (Symbol)argNames.getCar();
        FirstClassObject value =
                argValues.getCar();
        e.define( name, value );

        return () -> _bind( e, (Cons)argNames.getCdr(), (Cons)argValues.getCdr(), c );
    }

    /**
     * Set this operation's symbolic name.  Default name is 'anonymous'.
     *
     * @param name The symbolic name for the operation.
     */
    public Operation setName( Symbol name )
    {
        _name = name;
        return this;
    }

    /**
     * Get this {@code Operation}'s symbolic name.
     *
     * @return The operation's symbolic name.  If no name has been set then this
     *         will be 'anonymous'.
     * @see Operation#setName
     */
    public Symbol getName()
    {
        return _name;
    }

    /**
     * Checks if the length of the actual argument list is the length we expect.
     *
     * @param expected The expected number of arguments.
     * @param received The array of arguments received.
     * @throws RuntimeX If the number of arguments was wrong.
     */
    static protected void checkArgumentCount(
            int expected,
            FirstClassObject[] received )
                    throws RuntimeX
    {
        if ( expected != received.length )
            throw Raise.mWrongNumberOfArguments(
                    expected,
                    received.length
                    );
    }

    static protected void checkArgumentCount(
            int expected,
            Cons received )
                    throws RuntimeX
    {
        checkArgumentCount(
                expected,
                Cons.asArray( received ) );
    }
    static protected long checkArgumentCount(
            int min,
            int max,
            Cons received )
                    throws RuntimeX
    {
        long argumentCount =
                Cons.length( received );

        if ( argumentCount < min )
        {
            throw Raise.mNotEnoughArguments(
                    min,
                    argumentCount );
        }
        if ( argumentCount > max )
        {
            throw Raise.mTooManyArguments(
                    max,
                    argumentCount );
        }

        return argumentCount;
    }

    /**
     * Checks if the length of the actual argument list is the length
     * we expect.
     *
     * @param expected The expected number of arguments.
     * @param received The array of arguments received.
     * @throws RuntimeX If the number of arguments was wrong.
     */
    protected void checkArgumentCount( Cons received )
            throws RuntimeX
    {
        var formalCount =
                Cons.length( _formalArguments );
        var receivedCount =
                Cons.length( received );

        if ( formalCount == receivedCount )
            return;
        if ( formalCount < receivedCount && isVariadic() )
            return;

        throw Raise.mWrongNumberOfArgumentsF( getName(), formalCount, receivedCount );
    }

    /**
     * Checks if the received argument types are the expected ones.
     *
     * @param position The argument number in the argument list.
     * @param formal The expected type's class.
     * @param received The actual object received.
     * @throws RuntimeX In case the wrong type was passed.
     */
    static final protected void checkArgument(
            int position,
            Class<? extends FirstClassObject> formal,
            FirstClassObject received )
                    throws RuntimeX
    {
        Scut.asNotNil( formal, received );
    }

    /**
     * Checks if the passed formals represent a valid argument list.
     * <p>
     * {@code r7rs 4.1.4 p13}
     *
     * @param formals The formals to check.
     * @throws RuntimeX If formals represent no valid argument list.
     */
    static final protected void checkFormals(
            FirstClassObject formals )
                    throws RuntimeX
    {
        if ( FirstClassObject.is( Symbol.class, formals ) )
            return;
        if ( FirstClassObject.is( Cons.class, formals ) )
            return;

        throw Raise.mInvalidFormals( formals );
    }

    /**
     * Holds the function implementation.
     * Override in Java-implemented Operations.
     *
     * @param e
     * @param args
     * @param c
     * @return
     * @throws RuntimeX
     */
    protected Thunk _executeImpl( Environment e, Cons args, Cont<FirstClassObject> c )
            throws RuntimeX
    {
        checkArgumentCount( args );

        final var ex = e.extend( getName() );

        if ( isVariadic() )
            ex.define( _rest, Cons.NIL );

        return () -> _bind(
                ex,
                _formalArguments,
                args,
                env ->Primitives._begin( env, _body, c ) );
    }

    /**
     * Holds the function implementation. This must be only overridden by the
     * implementation of Procedure.
     *
     * @param e
     * @param args
     * @param c
     * @return
     * @throws RuntimeX
     */
    protected Thunk execute(
            Environment e,
            Cons args,
            Cont<FirstClassObject> c )
                    throws RuntimeX
    {
        return _executeImpl( e, args, c );
    }

    /**
     * Returns A lambda that calls _execute.
     * @param env
     * @param args
     * @return
     * @throws RuntimeX
     */
    protected final Lambda _compile( Environment env, Cons args ) throws RuntimeX
    {
        // See #160
        throw Raise.mInternalError( getClass() );
    }

    /**
     * @return A string representation of this object.
     */
    @Override
    public String toString()
    {
        return  String.format(
                "#<%s %s(%d)>",
                typename(),
                getName(),
                id() );
    }

    @Override
    public Object toJava() throws RuntimeX
    {
        return this;
    }
}

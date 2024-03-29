/*
 * Scream @ https://github.com/urschleim/scream
 *
 * Copyright © 1998-2022 Michael G. Binz
 */
package de.michab.scream.fcos;

import de.michab.scream.RuntimeX;
import de.michab.scream.util.Continuation.Cont;
import de.michab.scream.util.Continuation.Thunk;

/**
 * A lambda encapsulation for an fco.
 *
 * @author micbinz
 */
public class Lambda extends FirstClassObject
{
    @FunctionalInterface
    public static interface L {
        Thunk accept(Environment e, Cont<FirstClassObject> c) throws RuntimeX;
    }

    /**
     * The actual lambda closure.
     */
    private final L _l;

    /**
     * A name.
     */
    private final String _name;

    /**
     * Context info for debugging.
     */
    private Cons _info;

    public Lambda( L l, String name )
    {
        _l = l;
        _name = name;
    }
    public Lambda( L l, Symbol name )
    {
        this( l, name.toString() );
    }
    public Lambda( L l )
    {
        this( l, "anonymous" );
    }

    @Override
    public Thunk evaluate( Environment e, Cont<FirstClassObject> c )
    {
        return () -> _l.accept( e, c );
    }

    @Override
    public String toString()
    {
        return  String.format(
                "<%s %s id=%d info=\"%s\">",
                typename(),
                _name,
                id(),
                _info );
    }

    public Lambda setInfo( Cons c )
    {
        _info = c;
        return this;
    }
}

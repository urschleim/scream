/*
 * Scream @ https://github.com/urschleim/scream
 *
 * Copyright © 1998-2023 Michael G. Binz
 */
package de.michab.scream.pops;

import de.michab.scream.Raise;
import de.michab.scream.RuntimeX;
import de.michab.scream.fcos.Cons;
import de.michab.scream.fcos.Environment;
import de.michab.scream.fcos.FirstClassObject;
import de.michab.scream.fcos.Procedure;
import de.michab.scream.fcos.Symbol;
import de.michab.scream.fcos.Syntax;
import de.michab.scream.util.Continuation.Cont;
import de.michab.scream.util.Continuation.Thunk;
import de.michab.scream.util.Scut;

/**
 * {code (define <variable> <expression>)} syntax<br>
 * {code (define (<variable> <formals>) <body>)} syntax<br>
 * {code (define (<variable> . <formal>) <body>)} syntax<br>
 * <p>
 * {@code r7rs 5.3 p25 }
 */
public class SyntaxDefine extends Syntax
{
    private SyntaxDefine()
    {
        super( "define" );
    }

    @Override
    protected Thunk _executeImpl( Environment e, Cons args,
            Cont<FirstClassObject> c ) throws RuntimeX
    {
        checkArgumentCount( 2, Integer.MAX_VALUE, args );

        // TODO assert non circular
        var first =
                args.listRef( 0 );
        var second =
                args.listRef( 1 );

        if ( first instanceof Symbol )
        {
            return Primitives._eval(
                    e,
                    second,
                    fco -> Primitives._define(
                            e,
                            (Symbol)first,
                            fco,
                            ignored -> c.accept( Cons.NIL ) ) );
        }

        // This is a lambda define...

        Cons signature = Scut.as(
                Cons.class,
                first,
                s-> {
                    throw Raise.mSyntaxError();
                } );

        Symbol name = Scut.as(
                Symbol.class,
                signature.listRef( 0 ),
                s -> {
                    throw Raise.mDefineError();
                } );
        FirstClassObject parameterList =
                signature.getCdr();
        Cons body = Scut.as(
                Cons.class,
                args.listTail( 1 ) );

        return () -> {
            var value = new Procedure(
                    e,
                    parameterList,
                    body ).setName( name );
            return Primitives._define(
                    e,
                    name,
                    value,
                    ignore -> c.accept( Cons.NIL ) );
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
        tle.setPrimitive( new SyntaxDefine() );

        return tle;
    }
}

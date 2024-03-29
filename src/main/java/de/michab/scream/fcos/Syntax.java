/*
 * Scream @ https://github.com/urschleim/scream
 *
 * Copyright © 1998-2022 Michael G. Binz
 */
package de.michab.scream.fcos;

import de.michab.scream.RuntimeX;

/**
 * Followed the scheme spec in naming this class.  An alternate name would be
 * 'macro', also common is 'special form'.
 *
 * @author Michael Binz
 */
public class Syntax
    extends Operation
{
    /**
     * The name of the type as used by error reporting.
     *
     * @see FirstClassObject#typename()
     */
    public static final String TYPE_NAME = "syntax";

    /**
     * Default constructor.  Used for Java-defined specializations.
     *
     * @param name The new syntax' symbolic name.
     */
    private Syntax( Symbol name )
    {
        super( name );
    }

    /**
     * Constructor for Java-defined specializations.  The passed string names
     * the symbol bound to this syntax.
     *
     * @param name The string name of the symbol bound to the new syntax.  Used
     *        for error reporting.
     */
    protected Syntax( String name )
    {
        this( Symbol.createObject( name ) );
    }

    /**
     *
     * @param e
     * @param args
     * @param body
     * @throws RuntimeX
     */
    public Syntax(
            FirstClassObject args,
            Cons body  )
                    throws RuntimeX
    {
        super( args, body );
    }
}

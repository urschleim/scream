package de.michab.scream.pops;

import de.michab.scream.Cons;
import de.michab.scream.Environment;
import de.michab.scream.FirstClassObject;
import de.michab.scream.RuntimeX;
import de.michab.scream.ScreamException.Code;
import de.michab.scream.Symbol;
import de.michab.scream.Syntax;

/**
 * <code>
 * (do ((<variable1> <init1> <step1>)
 *      ... )
 *     (<test> <expression> ... )
 *     command ... )
 * </code><br>
 * Do is an iteration construct. It specifies a set of variables to be bound,
 * how they are to be initialized at the start, and how they are to be
 * updated on each iteration. When a termination condition is met, the loop
 * exits after evaluating the <code>expression</code>s.<br>
 * Do expressions are evaluated as follows: The <code>init</code> expressions
 * are evaluated (in some unspecifed order), the <code>variable</code>s are
 * bound to fresh locations, the results of the <code>init</code> expressions
 * are stored in the bindings of the <code>variable</code>s, and then the
 * iteration phase begins.<br>
 * Each iteration begins by evaluating <code>test</code>; if the result is
 * false (see section 6.3.1), then the <code>command</code> expressions are
 * evaluated in order for effect, the <code>step</code> expressions are
 * evaluated in some unspecified order, the <code>variable</code>s are bound
 * to fresh locations, the results of the <code>step</code>s are stored in
 * the bindings of the <code>variable</code>s, and the next iteration
 * begins.<br>
 * If <code>test</code> evaluates to a true value, then the
 * <code>expression</code>s are evaluated from left to right and the value(s)
 * of the last <code>expression<code> is(are) returned. If no
 * <code>expression<code>s are present, then the value of the do expression
 * is unspecified.<br>
 * The region of the binding of a <code>variable</code> consists of the
 * entire do expression except for the <code>init<code>s. It is an error for
 * a <code>variable<code> to appear more than once in the list of do
 * variables.<br>
 * A <code>step<code> may be omitted, in which case the effect is the same as
 * if <code>(<variable> <init> <variable>)</code> had been written instead of
 * <code>(<variable> <init>)</code>.
 */
public class SyntaxDo extends Syntax
{
    private SyntaxDo()
    {
        super( "do" );
    }

    /**
     * Checks if the passed argument is a <code>Cons</code>, is not NIL and is
     * a proper list.  Transforms the list into an array and returns that.
     * @throws RuntimeX
     */
    private FirstClassObject[] isNonNilAndProper( FirstClassObject fco ) throws RuntimeX
    {
        if ( Cons.NIL == fco )
            throw new ClassCastException();
        if ( ! ((Cons)fco).isProperList() )
            throw new ClassCastException();
        return ((Cons)fco).asArray();
    }

    @Override
    public FirstClassObject compile( Environment parent, FirstClassObject[] args )
            throws RuntimeX
    {
        checkMinimumArgumentCount( 2, args );

        try
        {
            FirstClassObject[] bindings = isNonNilAndProper( args[0] );

            Symbol[] vars = new Symbol[ bindings.length ];
            FirstClassObject[] inits = new FirstClassObject[ vars.length ];
            FirstClassObject[] steps = new FirstClassObject[ vars.length ];

            for ( int i = 0 ; i < vars.length ; i++ )
            {
                FirstClassObject[] binding = isNonNilAndProper( bindings[i] );

                if ( Cons.NIL == binding[0] )
                    throw new ClassCastException();

                switch ( binding.length )
                {
                case 2:
                    vars[i] = (Symbol)binding[0];
                    inits[i] = compile( binding[1], parent );
                    steps[i] = vars[i];
                    break;
                case 3:
                    vars[i] = (Symbol)binding[0];
                    inits[i] = compile( binding[1], parent );
                    steps[i] = compile( binding[2], parent );
                    break;

                default:
                    throw new ClassCastException();
                }
            }

            FirstClassObject[] testSequence = isNonNilAndProper( args[1] );
            for ( int i = 0 ; i < testSequence.length ; i++ )
                testSequence[i] = compile( testSequence[i], parent );

            FirstClassObject test = testSequence[0];

            FirstClassObject[] exps = new FirstClassObject[ testSequence.length -1 ];
            System.arraycopy( testSequence, 1, exps, 0, exps.length );

            FirstClassObject[] cmds = new FirstClassObject[ args.length -2 ];
            System.arraycopy( args, 2, cmds, 0, cmds.length );
            for ( int i = 0 ; i < cmds.length ; i++ )
                cmds[i] = compile( cmds[i], parent );

            return new Loop( vars, inits, steps, test, exps, cmds );
        }
        catch ( ClassCastException e )
        {
            throw new RuntimeX( Code.BAD_BINDING );
        }
    }

    /**
     * Base operations setup.
     *
     * @param tle A reference to the top level environment to be extended.
     * @return The extended environment.
     */
    public static Environment extendTopLevelEnvironment( Environment tle )
    {
        tle.setPrimitive( new SyntaxDo() );

        return tle;
    }
}

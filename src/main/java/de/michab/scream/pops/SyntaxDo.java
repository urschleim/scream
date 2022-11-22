package de.michab.scream.pops;

import de.michab.scream.Cons;
import de.michab.scream.Environment;
import de.michab.scream.FirstClassObject;
import de.michab.scream.Lambda;
import de.michab.scream.RuntimeX;
import de.michab.scream.Symbol;
import de.michab.scream.Syntax;
import de.michab.scream.util.Scut;

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
     *
     * @param variables {@code ((<var1> <init1> opt<step1>) ... )}
     * @return Two lists: The car-list contains the init-expressions.  The cdr-list
     * contains the step-expressions.
     * @throws RuntimeX
     */
    private Cons validateVariables( Cons variables ) throws RuntimeX
    {

        Cons inits = Cons.NIL;
        Cons steps = Cons.NIL;

        while ( variables != Cons.NIL )
        {
            var variableDef =
                    Scut.as( Cons.class, variables.getCar() );
            checkArgumentCount( 2, 3, variableDef );

            Symbol variable =
                    Scut.as( Symbol.class, variableDef.listRef( 0 ) );

            // Add a new init pair.
            inits = new Cons(
                    Cons.create(
                            variable,
                            variableDef.listRef( 1 )), inits );

            if ( variableDef.length() == 3 )
            {
                // Add a new step pair.
                steps = new Cons(
                        Cons.create(
                                variable,
                                variableDef.listRef( 2 )), steps );
            }

            variables = (Cons)variables.getCdr();
        }

        return new Cons( inits, steps );
    }

    @Override
    protected Lambda _compile( Environment env, Cons args ) throws RuntimeX
    {
        checkArgumentCount( 2, Integer.MAX_VALUE, args );

        var variables =
                Scut.as( Cons.class, args.listRef( 0 ) );
        var test =
                Scut.as( Cons.class, args.listRef( 1 ) );
        // May be nil.
        var commands =
                args.listTail( 2 );

        var setup =
                validateVariables( variables );

        checkArgumentCount( 1, Integer.MAX_VALUE, test );


        return new Lambda(
                (e,c) -> Continuation._x_do(
                        e,
                        (Cons)setup.getCar(),
                        (Cons)setup.getCdr(),
                        test,
                        commands,
                        c ),
                this.getName() ).setInfo( args );
    }

    @Override
    public FirstClassObject compile( Environment parent, Cons args )
            throws RuntimeX
    {
        return _compile( parent, args );
    }
    @Override
    public FirstClassObject activate( Environment parent,
            Cons arguments )
                    throws RuntimeX
    {
        var λ = _compile( parent, arguments );

        return FirstClassObject.evaluate( λ, parent );
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


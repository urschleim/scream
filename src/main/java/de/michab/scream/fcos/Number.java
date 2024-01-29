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
import de.michab.scream.util.Scut;

/**
 * The base class for Scream's numeric types.
 */
public abstract class Number
extends FirstClassObject
{
    /**
     * The name of the type as used by error reporting.
     *
     * @see FirstClassObject#typename()
     */
    public static final String TYPE_NAME = "number";

    /**
     * Type save enumeration class.
     */
    private static enum ComparisonType
    {
        EQ, LT, LET, GT, GET
    };

    public static final ComparisonType EQ = ComparisonType.EQ;
    public static final ComparisonType LT = ComparisonType.LT;
    public static final ComparisonType LET = ComparisonType.LET;
    public static final ComparisonType GT = ComparisonType.GT;
    public static final ComparisonType GET = ComparisonType.GET;

    /**
     * The exactness flag as described in the scheme standard.  Not really
     * implemented.  The flag is always false.
     */
    private final boolean _isExact;

    /**
     * A predicate for the exactness flag as specified by the scheme standard.
     *
     * @return Whether this represents an exact or inexact number.
     */
    final public boolean isExact()
    {
        return _isExact;
    }

    /**
     * Create a number instance with a given exactness.
     *
     * @param isExact Specifies if the number is exact or inexact.
     */
    Number( boolean isExact )
    {
        _isExact = isExact;
        setConstant();
    }

    /**
     * Default constructor.  Since this is an abstract class this is only
     * implicitly called from an derived class implementing {@code Number}.
     */
    Number()
    {
        this( false );
    }

    /**
     * Returns this number's value as long.
     *
     * @return This number's value as long.
     */
    public abstract long asLong();



    /**
     * Returns this number's value as double.
     *
     * @return This number's value as double.
     */
    public abstract double asDouble();


    @FunctionalInterface
    interface ArithmeticOperation
    {
        Thunk perform(
                Environment e,
                Cons list,
                long listLength,
                Cont<FirstClassObject> c ) throws RuntimeX;
    }

    /**
     * Implements r7rs {@code (= y z)}.
     *
     * @param z
     * @return
     * @throws RuntimeX
     */
    public abstract boolean r7rsEqual( Number z )
            throws RuntimeX;
    public abstract boolean r7rsLessThan( Number z )
            throws RuntimeX;
    public abstract boolean r7rsGreaterThan( Number z )
            throws RuntimeX;
    public abstract boolean r7rsLessOrEqualThan( Number z )
            throws RuntimeX;
    public abstract boolean r7rsGreaterOrEqualThan( Number z )
            throws RuntimeX;

    /**
     * Implements r7rs {@code (exact z)}.
     *
     * @param z
     * @return
     * @throws RuntimeX
     */
    public Number r7rsExact()
      throws RuntimeX
    {
        if ( isExact() )
            return this;

        if ( asLong() == asDouble() )
            return SchemeInteger.createObject( asLong() );

        throw RuntimeX.mIllegalArgument( toString() );
    }

    /**
     * Computes this plus the argument.
     *
     * @param other The corresponding number to add.
     * @return The sum of this and other.
     * @throws RuntimeX In case an error occurred.
     */
    public abstract Number add( FirstClassObject other )
            throws RuntimeX;

    private static Thunk _add( Environment e, Number total, Cons rest, Cont<FirstClassObject> c )
            throws RuntimeX
    {
        if ( rest == Cons.NIL )
            return c.accept( total );

        var current = Scut.as( Number.class, rest.getCar() );

        return _add(
                e,
                total.add( current ),
                Scut.as( Cons.class, rest.getCdr() ),
                c );
    }

    // r7rs p. 36
    private static Thunk _x_add(
            Environment e,
            Cons list,
            long listLength,
            Cont<FirstClassObject> c )
    {
        var zero = SchemeInteger.createObject( 0 );

        if ( listLength == 0 )
            return () -> Primitives._quote( zero, c );

        return () -> _add(
                e,
                zero,
                list,
                c );
    }

    /**
     * Computes this - other.
     *
     * @param other The corresponding number to subtract.
     * @return The difference between this and other.
     * @throws RuntimeX In case an error occurred.
     */
    public abstract Number subtract( FirstClassObject other )
            throws RuntimeX;

    private static Thunk _subtract( Environment e, Number total, Cons rest, Cont<FirstClassObject> c )
            throws RuntimeX
    {
        if ( rest == Cons.NIL )
            return c.accept( total );

        var current = Scut.as( Number.class, rest.getCar() );

        return _subtract(
                e,
                total.subtract( current ),
                Scut.as( Cons.class, rest.getCdr() ),
                c );
    }

    private static Thunk _x_subtract(
            Environment e,
            Cons list,
            long listLength,
            Cont<FirstClassObject> c ) throws RuntimeX
    {
        if ( listLength == 1 )
            return () -> _subtract(
                    e,
                    SchemeInteger.createObject( 0 ),
                    list,
                    c );

            return () -> _subtract(
                    e,
                    Scut.as( Number.class, list.getCar() ),
                    Scut.as( Cons.class, list.getCdr() ),
                    c );
    }

    /**
     * Computes this * other.
     * @param other The corresponding number to multiply.
     * @return The product of this and other.
     * @throws RuntimeX In case an error occurred.
     */
    public abstract Number multiply( FirstClassObject other )
            throws RuntimeX;

    private static Thunk _multiply( Environment e, Number total, Cons rest, Cont<FirstClassObject> c )
            throws RuntimeX
    {
        if ( rest == Cons.NIL )
            return c.accept( total );

        var current = Scut.as( Number.class, rest.getCar() );

        return _multiply(
                e,
                total.multiply( current ),
                Scut.as( Cons.class, rest.getCdr() ),
                c );
    }

    private static Thunk _x_multiply(
            Environment e,
            Cons list,
            long listLength,
            Cont<FirstClassObject> c ) throws RuntimeX
    {
        var one = SchemeInteger.createObject( 1 );

        if ( listLength == 0 )
            return () -> Primitives._quote( one, c );

        return () -> _multiply(
                e,
                one,
                list,
                c );
    }

    /**
     * Computes this / other.
     * @param other The corresponding number to divide.
     * @return The quotient of this and other.
     * @throws RuntimeX In case an error occurred.
     */
    public final Number divide( FirstClassObject other )
            throws RuntimeX
    {
        if ( other == Cons.NIL )
            throw RuntimeX.mTypeError( Number.class, other );

        double otherDouble =
                other.as( Number.class ).asDouble();

        if ( 0.0 == otherDouble )
            throw RuntimeX.mDivisionByZero();

        return SchemeDouble.createObject(
                asDouble() / otherDouble );
    }

    private static Thunk _divide( Environment e, Number total, Cons rest, Cont<FirstClassObject> c )
            throws RuntimeX
    {
        if ( rest == Cons.NIL )
            return c.accept( total );

        var current = Scut.as( Number.class, rest.getCar() );

        try {
            return _divide(
                    e,
                    total.divide( current ),
                    Scut.as( Cons.class, rest.getCdr() ),
                    c );
        }
        catch ( ArithmeticException aex )
        {
            throw RuntimeX.mDivisionByZero();
        }
    }

    private static Thunk _x_divide(
            Environment e,
            Cons list,
            long listLength,
            Cont<FirstClassObject> c ) throws RuntimeX
    {
        if ( listLength == 1 )
            return () -> _divide(
                    e,
                    SchemeDouble.createObject( 1 ),
                    list,
                    c );

        return () -> _divide(
                e,
                Scut.as( Number.class, list.getCar() ),
                Scut.as( Cons.class, list.getCdr() ),
                c );
    }

    private static Thunk doArithmetic(
            ArithmeticOperation opr,
            Environment e,
            Cons args,
            long argsLength,
            Cont<FirstClassObject> c )
        throws RuntimeX
    {
        return opr.perform(
                e,
                args,
                argsLength,
                c );
    }

    /**
     * (+ ...
     */
    static private Procedure addProc( Environment e )
    {
        return new Procedure( "+", e )
        {
            @Override
            protected Thunk _executeImpl( Environment e, Cons args, Cont<FirstClassObject> c )
                    throws RuntimeX
            {
                var len = checkArgumentCount( 0, Integer.MAX_VALUE, args );

                return doArithmetic(
                        Number::_x_add,
                        e,
                        args,
                        len,
                        c );
            }
        };
    }

    /**
     * (- ...
     */
    static private Procedure subtractProc( Environment e )
    {
        return new Procedure( "-", e )
        {
            @Override
            protected Thunk _executeImpl( Environment e, Cons args, Cont<FirstClassObject> c )
                    throws RuntimeX
            {
                var len = checkArgumentCount( 1, Integer.MAX_VALUE, args );

                return doArithmetic(
                        Number::_x_subtract,
                        e,
                        args,
                        len,
                        c );
            }
        };
    }

    /**
     * (* ...
     */
    static private Procedure multiplyProc( Environment e )
    {
        return new Procedure( "*", e )
        {
            @Override
            protected Thunk _executeImpl( Environment e, Cons args, Cont<FirstClassObject> c )
                    throws RuntimeX
            {
                var len = checkArgumentCount( 0, Integer.MAX_VALUE, args );

                return doArithmetic(
                        Number::_x_multiply,
                        e,
                        args,
                        len,
                        c );
            }

        };
    }

    /**
     * (/ ...
     */
    static private Procedure divideProc( Environment e ) {
        return new Procedure( "/", e )
        {
            @Override
            protected Thunk _executeImpl( Environment e, Cons args, Cont<FirstClassObject> c )
                    throws RuntimeX
            {
                var len = checkArgumentCount( 1, Integer.MAX_VALUE, args );

                // Check after the division if it is possible to convert the
                // result to integer.
                Cont<FirstClassObject> finish = fco -> {
                    var result = (SchemeDouble)fco;
                    if ( Math.round( result.asDouble() ) == result.asDouble() )
                        return c.accept( SchemeInteger.createObject( result.asLong() ) );
                    return c.accept( fco );
                };

                return doArithmetic(
                        Number::_x_divide,
                        e,
                        args,
                        len,
                        finish );
            }
        };
    }

    /**
     * Number operations setup.
     *
     * @param tle The toplevel-environment to extend.
     * @return The extended environment.
     */
    public static Environment extendTopLevelEnvironment( Environment tle )
            throws RuntimeX
    {
        tle.setPrimitive( addProc( tle ) );
        tle.setPrimitive( subtractProc( tle ) );
        tle.setPrimitive( multiplyProc( tle ) );
        tle.setPrimitive( divideProc( tle ) );

        return tle;
    }
}

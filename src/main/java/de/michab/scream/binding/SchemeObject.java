/*
 * Scream @ https://github.com/urschleim/scream
 *
 * Copyright © 1998-2024 Michael G. Binz
 */
package de.michab.scream.binding;

import java.lang.reflect.Array;
import java.lang.reflect.Executable;
import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.util.Objects;
import java.util.logging.Logger;

import org.smack.util.JavaUtil;

import de.michab.scream.RuntimeX;
import de.michab.scream.ScreamEvaluator;
import de.michab.scream.fcos.Bool;
import de.michab.scream.fcos.Cons;
import de.michab.scream.fcos.Environment;
import de.michab.scream.fcos.FirstClassObject;
import de.michab.scream.fcos.Int;
import de.michab.scream.fcos.Number;
import de.michab.scream.fcos.Procedure;
import de.michab.scream.fcos.Real;
import de.michab.scream.fcos.SchemeCharacter;
import de.michab.scream.fcos.SchemeString;
import de.michab.scream.fcos.Symbol;
import de.michab.scream.fcos.Syntax;
import de.michab.scream.fcos.Vector;
import de.michab.scream.pops.Primitives;
import de.michab.scream.util.Continuation.Cont;
import de.michab.scream.util.Continuation.Thunk;
import de.michab.scream.util.ConversionFailedX;
import de.michab.scream.util.Scut;

/**
 * An instance of this class boxes an entity from the Java object system,
 * e.g. an object or a class. This wraps the object and implements the
 * Scheme/Java call mapping in both directions.
 *
 * @author Michael G. Binz
 */
public class SchemeObject
    extends Syntax
{
    /**
     * The logger for this class.
     */
    private final static Logger LOG =
            Logger.getLogger( SchemeObject.class.getName() );

    /**
     * The name of the type as used by error reporting.
     *
     * @see FirstClassObject#typename()
     */
    public static final String TYPE_NAME = "object";

    /**
     * The instance that is managed.  Note that this must never be null.
     */
    private final java.lang.Object _theInstance;

    /**
     * {@code True} if this object represents a Java class,
     * {@code false} otherwise.
     */
    private final boolean _isClass;

    /**
     * The instance's class adapter.
     */
    private final JavaClassAdapter _classAdapter;

    /**
     * Create a new SchemeObject for a given object.  In case the
     * passed object instance is {@code null} the
     * instance will be set to the class adapter's embedded class.
     * <p>
     * The resulting SchemeObject then represents a class.
     *
     * @param object The object to be wrapped by the new instance.
     * @param adapter The class adapter for the new instance.
     */
    private SchemeObject( java.lang.Object object, JavaClassAdapter adapter )
    {
        super( adapter.adapterFor().getName() );

        if ( object == null )
        {
            _isClass = true;
            _theInstance = adapter.adapterFor();
        }
        else
        {
            _isClass = false;
            _theInstance = object;
        }

        _classAdapter = adapter;
    }

    /**
     * Create a new SchemeObject for a given object.
     *
     * @param object The object to be wrapped by the new instance.
     */
    private SchemeObject( java.lang.Object object )
    {
        this( object, JavaClassAdapter.get( object.getClass() ) );
    }

    /**
     * Wrap the passed object as a SchemeObject.  If null is passed then
     * null is returned.  If the passed object is already a SchemeObject
     * then this is returned.
     *
     * @param object The object to wrap.
     * @return A new SchemeObject or null if null was passed.
     */
    public static SchemeObject make( Object object )
    {
        if ( object == null )
            return null;

        if ( object instanceof SchemeObject )
            return (SchemeObject)object;

        return new SchemeObject( object );
    }

    private static Thunk createClass(
            String classname,
            Cont<FirstClassObject> c )
                    throws RuntimeX
    {
        var x = new SchemeObject(
                null,
                JavaClassAdapter.get( classname ) );

        return c.accept( x );
    }

    private static Thunk createObjectExt(
            String name,
            FirstClassObject[] ctorArgs,
            Cont<FirstClassObject> c )
                    throws RuntimeX
    {
        Objects.requireNonNull(
                name );
        Objects.requireNonNull(
                ctorArgs );

        if ( name.length() == 0 )
            throw RuntimeX.mIllegalArgument( name );

        var ctor = JavaClassAdapter.getCtor( name );

        var result = JavaClassAdapter.createInstance(
                ctor,
                ctorArgs  );

        // No conversion to Fco.
        return c.accept(
                result instanceof FirstClassObject ?
                        FirstClassObject.class.cast( result ) :
                            new SchemeObject( result ) );
    }

    private static Thunk _createObjectExt(
            String className,
            Cons ctorArgs,
            Cont<FirstClassObject> c )
    {
        return () -> createObjectExt(
                className,
                Cons.asArray( ctorArgs ),
                c );
    }

    private static Thunk callExt(
            SchemeObject instance,
            String name,
            FirstClassObject[] args,
            Cont<SchemeObject> c )
                    throws RuntimeX
    {
        if ( name.length() == 0 )
            throw RuntimeX.mIllegalArgument( name );

        var classAdapter =
                JavaClassAdapter.get(
                        instance._isClass ?
                                (Class<?>)instance.toJava() :
                                instance.toJava().getClass() );

        var x = classAdapter.getMethod( name );

        var result = classAdapter.call(
                instance,
                x,
                args  );

        // No conversion to Fco.
        return c.accept(
                SchemeObject.make( result ) );
    }

    private Thunk _callExt(
            String methodName,
            Cons arguments,
            Cont<FirstClassObject> c )
    {
        return () -> callExt(
                this,
                methodName,
                Cons.asArray( arguments ),
                result -> _convertJava2Scream( result, c ) );
    }

    /**
     * @return The wrapped Java object.
     */
    @Override
    public Object toJava()
    {
        return _theInstance;
    }
    static public Object toJava( SchemeObject object )
    {
        if ( object == null )
            return object;

        return object._theInstance;
    }

    @Override
    protected Thunk _executeImpl(
            Environment e,
            Cons args,
            Cont<FirstClassObject> c )
                    throws RuntimeX
    {
        long argsLen =
                checkArgumentCount( 1, 2, args );

        var args0 = args.listRef( 0 );

        if ( argsLen == 1 && FirstClassObject.is( Cons.class, args0 ) )
        {
            return processInvocation(
                    e,
                    Scut.asNotNil( Cons.class, args0 ),
                    c );
        }

        if ( argsLen == 1 && args0 instanceof SchemeString )
        {
            return processAttributeGet(
                    ((SchemeString)args0).getValue(),
                    c );
        }

        if ( argsLen == 2 && args0 instanceof Symbol )
        {
            Symbol symbol = (Symbol)args0;
            var args1 = args.listRef( 1 );

            return Primitives._eval(
                    e,
                    args1,
                    fco -> _processAttributeSet( symbol, fco, c ) );
        }

        throw RuntimeX.mInternalError( SchemeObject.class.toString() );
    }

    /**
     * Matches the actual parameter list of the invocation with the passed
     * formal parameter list.
     *
     * @param formal An array of classes representing the formal parameter list.
     * @param actual An array of objects representing the actual parameters.
     * @return An array of objects converted from the scream types to java types
     *         suitable for invoking the method.  If no match was possible null
     *         is returned.
     */
    private static Object[] matchParameters(
            Class<?>[] formal,
            FirstClassObject[] actual )
    {
        // Check the length.
        if ( formal.length != actual.length )
            return null;

        try
        {
            Object[] result = new Object[ formal.length ];

            for ( int i = 0 ; i < result.length ; i++ )
                result[i] = convertScream2Java( formal[i], actual[i] );

            return result;
        }
        catch ( RuntimeX e )
        {
            return null;
        }
    }

    /**
     * Converts a FirstClassObject into an object suitable for inserting into an
     * argument list for a java method call.
     * <p>
     * Currently not all possible conversion are done.  It would be no problem to
     * change this method so it accepts a int scheme argument and converts this
     * to a Java double.  This wasn't done because then we need another order of
     * methods in JavaClassAdapter.  In JDK1.2 the method double abs( double ) is
     * contained in the method list of java.lang.Math long abs( long ).  As a
     * result the integer argument <i>always</i> was converted and there was no
     * possibility to call the integer method.
     * <p>
     * A similar example is the trivial boolean conversion that explicitly is
     * <i>not</i> carried out by this method:  In Scheme everything but #F is #T.
     * If we would implement this here, the result were that if by chance the
     * anyType method( boolean ); were the first in the classes method list we
     * never are able to call another one argument method, since all single arg
     * calls were catched by this.
     *
     * @param formal The class expected in the java argument list.
     * @param actual The FirstClassObject to convert.
     * @return An Object resulting from the conversion.  This is ready to be
     *          inserted into the argument list for an invoke() call.
     * @throws ConversionFailedX Is thrown if the conversion failed.
     */
    public static Object convertScream2Java(
            Class<?> formal,
            FirstClassObject actual )
                    throws
                    RuntimeX
    {
        try
        {
            Object result = null;

            // First we check the special case:  A NIL is converted to Java 'null'.
            if ( actual == Cons.NIL )
                result = null;

            // Check for java primitive types
            else if ( formal == java.lang.Boolean.TYPE )
                result = Boolean.valueOf( ((Bool)actual).getValue() );

            else if ( formal == java.lang.Byte.TYPE )
                result = Byte.valueOf( (byte)((Int)actual).asLong() );

            else if ( formal == java.lang.Short.TYPE )
                result = Short.valueOf( (short)((Int)actual).asLong() );

            else if ( formal == java.lang.Integer.TYPE )
                result = Integer.valueOf( (int)((Int)actual).asLong() );

            else if ( formal == java.lang.Long.TYPE )
                result = Long.valueOf( ((Int)actual).asLong() );

            else if ( formal == java.lang.Float.TYPE )
                result = Float.valueOf( (float)((Real)actual).asDouble() );

            else if ( formal == java.lang.Double.TYPE )
                result = Double.valueOf( ((Number)actual).asDouble() );

            else if ( formal == java.lang.Character.TYPE )
                result = Character.valueOf( ((SchemeCharacter)actual).asCharacter() );

            else if ( formal == java.lang.String.class )
                result = ((SchemeString)actual).getValue();

            // This handles the special case that a method is called that knows
            // Scream's internal type system.  Currently this is used by the
            // AwtListener class that directly accepts Cons lists.
            // Holy cow, this is one of the most complex code lines I ever wrote.
            // The condition is only taken if the formal argument is some kind of a
            // FirstClassObject and the passed actual argument can be assigned to
            // that type.
            else if ( FirstClassObject.class.isAssignableFrom( formal ) &&
                    formal.isAssignableFrom( actual.getClass() ) )
                result = actual;

            // Check for and convert arrays.
            else if ( formal.isArray() )
            {
                result = convertScreamVector2JavaArray(
                        formal,
                        (de.michab.scream.fcos.Vector)actual );
            }

            // The last chance conversion.
            else if ( actual instanceof FirstClassObject )
            {
                // According to the new rules in this class the instance must never be
                // null.
                Object actualValue = actual.toJava();

                if ( formal.isAssignableFrom( actualValue.getClass() ) )
                    result = actualValue;
                else
                    throw new ConversionFailedX( actual, formal );
            }

            else
                throw new ConversionFailedX( actual, formal );

            return result;
        }
        catch ( ClassCastException e )
        {
            throw new ConversionFailedX( actual, formal );
        }
    }

    /**
     * Converts a vector, something like {@code #(1 2 3 4)}, into a Java
     * array.
     *
     * @param formal The array type (a.k.a. component type) for the resulting
     *        array.
     * @param actual The scheme vector to convert.
     * @return The resulting array.
     * @throws ConversionFailedX In case there were type errors.
     */
    private static Object convertScreamVector2JavaArray(
            Class<?> formal,
            de.michab.scream.fcos.Vector actual )
                    throws
                    RuntimeX
    {
        var len = actual.size();
        Class<?> componentType = formal.getComponentType();

        // Create the result array.
        Object result = Array.newInstance( componentType, (int)len );

        for ( int i = 0 ; i < len ; i++ )
            Array.set( result, i, convertScream2Java(
                    componentType,
                    actual.get( i ) ) );

        return result;
    }

    /**
     * This method is responsible for handling
     * {@code InvocationTargetException}s.  This means that in
     * case the exception embedded in an {@code InvocationTargetException}
     * is a {@code RuntimeX} then this method unpacks and returns this.  In
     * the other case a new {@code RuntimeX} is created and returned.
     * Embedded {@code Error}s are unpacked and simply thrown.
     *
     * @param ite The exception to filter.
     * @param context A string to be used in case the embedded exception is not
     *        an {@code Error} or {@code RuntimeX} and a generic
     *        exception has to be created.
     * @return An instance of a {@code RuntimeX} exception.
     * @throws Error Embedded {@code Error} instances are thrown.
     */
    static RuntimeX filterException( InvocationTargetException ite,
            Executable context )
    {
        Throwable t = ite.getCause();
        JavaUtil.Assert( t != null );

        try
        {
            return (RuntimeX)t;
        }
        catch ( Exception ee )
        {
            return RuntimeX.mInvocationException(
                    context,
                    t );
        }
    }

    /**
     * Converts a Java object back into Scream's type system.
     *
     * @param object The object to be boxed.
     * @return The object representing the box.
     */
    public static FirstClassObject convertJava2Scream( java.lang.Object object )
    {
        // Java nulls are mapped into NIL.
        if ( null == object )
            return Cons.NIL;

        // Test for array types...
        if ( object.getClass().isArray() )
            return convertJavaArray2ScreamVector( object );

        // Now we test for all primitive types supported by Java.
        if ( object instanceof java.lang.Integer ||
                object instanceof java.lang.Byte ||
                object instanceof java.lang.Short ||
                object instanceof java.lang.Long )
            return Int.createObject( ((java.lang.Number)object).longValue() );

        if ( object instanceof java.lang.Double ||
                object instanceof java.lang.Float )
            return Real.createObject( ((java.lang.Number)object).doubleValue() );

        if ( object instanceof java.lang.Character )
            return SchemeCharacter.createObject( ((java.lang.Character)object).charValue() );

        if ( object instanceof String )
            return SchemeString.make( (String)object );

        if ( object instanceof java.lang.Boolean )
            return Bool.createObject( ((java.lang.Boolean)object).booleanValue() );

        // This is needed for tightly integrated classes that know about Scream's
        // internal type system.
        if ( object instanceof FirstClassObject )
            return (FirstClassObject)object;

        // Everything else must be a Java-native type.
        return new SchemeObject(
                object,
                JavaClassAdapter.get( object.getClass() ) );
    }

    private static Thunk _convertJava2Scream(
            SchemeObject object,
            Cont<FirstClassObject> c )
    {
        return ScreamEvaluator.CONT.get().toCont(
                () -> convertJava2Scream( SchemeObject.toJava( object ) ),
                c );
    }

    /**
     * Converts a Java array into a scream vector.  The passed object must be an
     * array, i.e. {@code Array.isArray()} must return true for it.  This is
     * not checked inside this method.
     *
     * @param object The object to convert to a vector.
     * @return The resulting vector.
     */
    private static Vector convertJavaArray2ScreamVector( java.lang.Object object )
    {
        FirstClassObject[] vector =
                new FirstClassObject[ Array.getLength( object ) ];
        for ( int i = vector.length -1 ; i >= 0 ; i-- )
            vector[i] = convertJava2Scream( Array.get( object, i ) );
        return new Vector( vector, false );
    }

    private Thunk processInvocationImpl(
            Environment env,
            String methodName,
            Cons list,
            Cont<FirstClassObject> c )
                    throws RuntimeX
    {
        Executable methodRef = null;

        try
        {
            FirstClassObject[] argArray = Cons.asArray( list );

            // Select the method to call.
            for ( var method : _classAdapter.getMethods( methodName, argArray.length  ) )
            {
                java.lang.Object[] argumentList =
                        matchParameters( method.getParameterTypes(), argArray );

                if ( null != argumentList )
                {
                    // Check if it is tried to invoke an instance method on a class
                    // object.  Since the VM in this case simply throws a NPE we have
                    // to check for this condition manually.
                    if ( _isClass && ! Modifier.isStatic( method.getModifiers() ) )
                        throw RuntimeX.mCannotAccessInstance();
                    // Store the method reference for the exception handler.
                    methodRef = method;
                    // Do the actual call.
                    return c.accept( convertJava2Scream(
                            method.invoke( _theInstance, argumentList ) ) );
                }
            }

            throw RuntimeX.mMethodNotFound( methodName, list );
        }
        catch ( InvocationTargetException e )
        {
            throw filterException( e, methodRef );
        }
        catch ( IllegalArgumentException e )
        {
            // Not sure if this can be thrown under normal circumstances.  The only
            // reason for that exception is if it is tried to invoke an
            // instance method on a java.lang.Class object.  In that case the illegal
            // argument is the first argument to invoke, that is on the one hand non
            // null, but on the other hand simply the wrong reference.
            throw RuntimeX.mInternalError( methodName );
        }
        catch ( IllegalAccessException e )
        {
            throw RuntimeX.mIllegalAccess( methodName );
        }
    }

    private Thunk _processInvocationImpl(
            Environment env,
            String methodName,
            Cons list,
            Cont<FirstClassObject> c )
    {
        return () -> processInvocationImpl( env, methodName, list, c );
    }

    /**
     * Processes a procedure invocation.
     *
     * @param env The environment used for the evaluation of the argument list.
     * @param list The arguments used for the invocation.
     * @return The result of the procedure invocation.
     * @throws RuntimeX In case there where access errors.
     */
    private Thunk processInvocation(
            Environment env,
            Cons list,
            Cont<FirstClassObject> c )
                    throws RuntimeX
    {
        if ( FirstClassObject.is( Symbol.class, list.getCar() ))
        {
            var symbol = Scut.as(
                    Symbol.class, list.getCar() );
            var rest = Scut.as(
                    Cons.class, list.getCdr() );

            return Primitives._evalCons(
                    env,
                    rest,
                    evaluated -> _processInvocationImpl(
                            env,
                            symbol.toString(),
                            evaluated,
                            c ) );
        }

        var string = Scut.as(
                SchemeString.class, list.getCar() );
        var rest = Scut.as(
                Cons.class, list.getCdr() );

        return Primitives._evalCons(
                env,
                rest,
                evaluated -> _callExt(
                        string.getValue(),
                        evaluated,
                        c ) );
    }

    /**
     * Get/Read an attribute from the passed object.
     *
     * @param attribute The attribute to read.
     * @param c The continuation receiving the attribute's value.
     * @return A thunk.
     * @throws RuntimeX In case the attribute could not be read.
     */
    private Thunk processAttributeGet( String attribute, Cont<FirstClassObject> c )
            throws RuntimeX
    {
        try
        {
            return c.accept( convertJava2Scream(
                    _classAdapter.getField( attribute ).get( _theInstance ) ) );
        }
        catch ( IllegalAccessException e )
        {
            throw RuntimeX.mIllegalAccess( attribute );
        }
    }

    /**
     * Sets an attribute on the passed object.
     *
     * @param attribute The attribute to set.  Attribute has to be of type
     *        symbol, in all other cases an exception will be thrown.
     * @param value The value to set.  This has to respect the type of the
     *        attribute parameter.
     * @return The attribute's new value.
     * @throws RuntimeX In case the attribute could not be set.
     */
    private Thunk processAttributeSet(
            Symbol attribute,
            FirstClassObject value,
            Cont<FirstClassObject> c )
                    throws RuntimeX
    {
        LOG.info( attribute + " = " + value );

        try
        {
            Field field = _classAdapter.getField(
                    attribute.toString() );

            field.set(
                    _theInstance,
                    convertScream2Java( field.getType(), value ) );

            return c.accept( value );
        }
        catch ( IllegalAccessException e )
        {
            throw RuntimeX.mCannotModifyConstant( attribute );
        }
    }

    private Thunk _processAttributeSet(
            Symbol attribute,
            FirstClassObject value,
            Cont<FirstClassObject> c )
    {
        return () -> processAttributeSet( attribute, value, c );
    }

    /**
     * Test for equality based on the embedded instance.  If we have no instance
     * which is the case for class representatives test if the same
     * {@code JavaClassAdapter} is used.
     *
     * @param other The object to compare.
     * @return {@code True} if the reference passed into this call is eqv to
     *         this object.
     */
    @Override
    public boolean eqv( FirstClassObject other )
    {
        try
        {
            SchemeObject otherSo = (SchemeObject)other;

            return _theInstance.equals( otherSo._theInstance );
        }
        catch ( ClassCastException e )
        {
            return false;
        }
    }

    /**
     * @return A string representation of this object.
     */
    @Override
    public String toString()
    {
        return "@Object:" + _theInstance.getClass().getName() + "=" + _theInstance;
    }

    /**
     * Clone this scheme object.  Makes a deep copy.
     *
     * @return The cloned object.
     * @see java.lang.Object#clone
     */
    @Override
    public SchemeObject copy()
    {
        throw new InternalError( getClass().getSimpleName() );
    }

    /**
     * Create an object or class representation.
     *
     * classname is a symbol.
     *
     * (make-object classname) -> class
     * (make-object (classname ...) -> instance ctor
     */
    static private Syntax constructObjectSyntax = new Syntax( "make-object" )
    {
        @Override
        protected Thunk _executeImpl( Environment e, Cons args,
                Cont<FirstClassObject> c ) throws RuntimeX
        {
            checkArgumentCount( 1, args );

            FirstClassObject argument = args.getCar();

            if ( argument instanceof SchemeString )
                return createClass( (String)argument.toJava(), c );

            Cons cons = Scut.as(
                    Cons.class,
                    argument );
            Cons arguments = Scut.as(
                    Cons.class,
                    cons.getCdr() );

            SchemeString name = Scut.as(
                    SchemeString.class,
                    cons.getCar() );

            return Primitives._evalCons(
                    e,
                    arguments,
                    evaluated ->
                    _createObjectExt(
                            name.getValue(),
                            evaluated,
                            object
                              -> {  return c.accept( convertJava2Scream( object ) ); }) );
        }
    };

    /**
     * (scream:java:make-instance "ctor-spec" ...)
     */
    static private Procedure scream_java_make_instance( Environment e )
    {
        return new Procedure( "scream:java:make-instance", e )
        {
            @Override
            protected Thunk _executeImpl( Environment e, Cons args,
                    Cont<FirstClassObject> c ) throws RuntimeX
            {
                if ( ! Cons.isProper( args ) )
                    throw RuntimeX.mExpectedProperList( args );

                checkArgumentCount(
                        1,
                        Integer.MAX_VALUE,
                        args );

                SchemeString ctor_spec =
                        Scut.asNotNil( SchemeString.class, args.getCar() );
                Cons parameters =
                        Scut.as( Cons.class, args.getCdr() );

                return createObjectExt(
                        ctor_spec.toJava(),
                        Cons.asArray( parameters),
                        c );
            }
        };
    }

    /**
     * (scream:java:make-class "class-name")
     *
     * Creates an object representing a class.
     */
    static private Procedure scream_java_make_class( Environment e )
    {
        return new Procedure( "scream:java:make-class", e )
        {
            @Override
            protected Thunk _executeImpl( Environment e, Cons args,
                    Cont<FirstClassObject> c ) throws RuntimeX
            {
                if ( ! Cons.isProper( args ) )
                    throw RuntimeX.mExpectedProperList( args );

                checkArgumentCount(
                        1,
                        args );

                SchemeString classname =
                        Scut.asNotNil( SchemeString.class, args.getCar() );

                return createClass( classname.toJava(), c );
            }
        };
    }

    /**
     * (scream:java:call object "meth-spec" ...)
     *
     * Calls the operation specified by meth-spec on object.
     * If the object represents a class then the static method is called.
     */
    static private Procedure scream_java_call( Environment e )
    {
        return new Procedure( "scream:java:call", e )
        {
            @Override
            protected Thunk _executeImpl( Environment e, Cons args,
                    Cont<FirstClassObject> c ) throws RuntimeX
            {
                if ( ! Cons.isProper( args ) )
                    throw RuntimeX.mExpectedProperList( args );

                checkArgumentCount(
                        2,
                        Integer.MAX_VALUE,
                        args );

                SchemeObject instance =
                        Scut.asNotNil( SchemeObject.class, args.getCar() );
                args =
                        Scut.as( Cons.class, args.getCdr() );
                SchemeString arg_spec =
                        Scut.asNotNil( SchemeString.class, args.getCar() );
                Cons parameters =
                        Scut.as( Cons.class, args.getCdr() );

                return callExt(
                        instance,
                        arg_spec.getValue(),
                        Cons.asArray( parameters),
                        so -> c.accept( so ) );
            }
        };
    }

    /**
     * (scream:java:to-fco object)
     *
     * Converts the passed object back into a Scheme-type.
     * If the passed object is already a Scheme-type the object is returned
     * unmodified.
     */
    static private Procedure scream_java_to_fco( Environment e )
    {
        return new Procedure( "scream:java:to-fco", e )
        {
            @Override
            protected Thunk _executeImpl( Environment e, Cons args,
                    Cont<FirstClassObject> c ) throws RuntimeX
            {
                if ( ! Cons.isProper( args ) )
                    throw RuntimeX.mExpectedProperList( args );

                checkArgumentCount(
                        1,
                        args );

                var a0 = args.getCar();

                if ( ! FirstClassObject.is( SchemeObject.class, a0 ) )
                    return c.accept( a0 );

                SchemeObject object =
                        Scut.asNotNil( SchemeObject.class, a0 );

                return c.accept( convertJava2Scream( object.toJava() ) );
            }
        };
    }

    /**
     * (object obj) -- Wraps the passed first class object with a scheme object.
     */
    static private Procedure wrapObjectProcedure( Environment e )
    {
        return new Procedure( "object", e )
        {
            @Override
            protected Thunk _executeImpl( Environment e, Cons args, Cont<FirstClassObject> c )
                    throws RuntimeX
            {
                checkArgumentCount( 1, args );

                return c.accept(
                        SchemeObject.make(
                                args.getCar() ) );
            }
        };
    }

    /**
     * (object? obj)
     */
    static private Procedure objectPredicateProcedure( Environment e )
    {
        return new Procedure( "object?", e )
        {
            @Override
            protected Thunk _executeImpl( Environment e, Cons args, Cont<FirstClassObject> c )
                    throws RuntimeX
            {
                checkArgumentCount( 1, args );

                return () -> c.accept(
                        Bool.createObject( args.getCar() instanceof SchemeObject ) );
            }
        };
    }

    /**
     * (describe-object obj) -> #f
     *
     * TODO: return a more scheme like representation...
     */
    static private Procedure describeObjectProcedure( Environment e )
    {
        return new Procedure( "describe-object", e )
        {
            @Override
            public Thunk _apply( Cons args, Cont<FirstClassObject> c )
                    throws RuntimeX
            {
                checkArgumentCount( 1, args );

                try
                {
                    SchemeObject so = Scut.as(
                            SchemeObject.class,
                            args.listRef( 0 ) );

                    int i;

                    Method[] methods =
                            so._classAdapter.getMethods();
                    FirstClassObject[] methodsV =
                            new FirstClassObject[ methods.length ];
                    for ( i = 0 ; i < methods.length ; i++ )
                        methodsV[i] = SchemeString.make( methods[i].toString() );

                    Field[] attributes = so._classAdapter.getFields();
                    FirstClassObject[] fieldsV = new FirstClassObject[ attributes.length ];
                    for ( i = 0 ; i < attributes.length ; i++ )
                        fieldsV[i] = SchemeString.make( attributes[i].toString() );

                    var result =
                            new Cons( new Vector( methodsV, false ),
                                    new Vector( fieldsV, false ) );

                    return c.accept( result );
                }
                catch ( ClassCastException e )
                {
                    throw RuntimeX.mInternalError();
                }
            }
        };
    }

    /**
     * Object operations setup.
     *
     * @param tle The environment to extend.
     * @return The extended environment.
     * @throws RuntimeX
     */
    public static Environment extendTopLevelEnvironment( Environment tle )
            throws RuntimeX
    {
        tle.setPrimitive( objectPredicateProcedure(tle) );
        tle.setPrimitive( wrapObjectProcedure( tle ) );
        tle.setPrimitive( constructObjectSyntax );

        tle.setPrimitive( scream_java_make_instance( tle ) );
        tle.setPrimitive( scream_java_call( tle ) );
        tle.setPrimitive( describeObjectProcedure( tle ) );
        tle.setPrimitive( scream_java_make_class( tle ) );
        tle.setPrimitive( scream_java_to_fco( tle ) );
        return tle;
    }
}

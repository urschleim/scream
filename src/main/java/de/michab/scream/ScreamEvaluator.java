/*
 * Scream @ https://github.com/urschleim/scream
 *
 * Copyright © 1998-2024 Michael G. Binz
 */
package de.michab.scream;

import java.io.IOException;
import java.io.Reader;
import java.io.StringReader;
import java.io.Writer;
import java.net.URL;
import java.util.Objects;
import java.util.Stack;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.script.Bindings;
import javax.script.ScriptContext;
import javax.script.ScriptEngine;
import javax.script.ScriptEngineFactory;
import javax.script.ScriptException;
import javax.script.SimpleScriptContext;

import org.smack.util.FunctionalUtil.FunctionX;
import org.smack.util.FunctionalUtil.SupplierX;
import org.smack.util.JavaUtil;

import de.michab.scream.binding.SchemeObject;
import de.michab.scream.fcos.Cons;
import de.michab.scream.fcos.Environment;
import de.michab.scream.fcos.FirstClassObject;
import de.michab.scream.fcos.Number;
import de.michab.scream.fcos.Port;
import de.michab.scream.fcos.PortIn;
import de.michab.scream.fcos.PortOut;
import de.michab.scream.fcos.Procedure;
import de.michab.scream.fcos.SchemeString;
import de.michab.scream.fcos.Symbol;
import de.michab.scream.fcos.Syntax;
import de.michab.scream.frontend.SchemeParser;
import de.michab.scream.pops.Exceptions_6_11;
import de.michab.scream.pops.PrimitiveProcedures;
import de.michab.scream.pops.Primitives;
import de.michab.scream.pops.REGS;
import de.michab.scream.pops.SyntaxAnd;
import de.michab.scream.pops.SyntaxAssign;
import de.michab.scream.pops.SyntaxBegin;
import de.michab.scream.pops.SyntaxCase;
import de.michab.scream.pops.SyntaxCond;
import de.michab.scream.pops.SyntaxDefine;
import de.michab.scream.pops.SyntaxDefineValues;
import de.michab.scream.pops.SyntaxDo;
import de.michab.scream.pops.SyntaxIf;
import de.michab.scream.pops.SyntaxLambda;
import de.michab.scream.pops.SyntaxLet;
import de.michab.scream.pops.SyntaxLetValues;
import de.michab.scream.pops.SyntaxOr;
import de.michab.scream.pops.SyntaxQuote;
import de.michab.scream.pops.SyntaxSyntax;
import de.michab.scream.pops.SyntaxTime;
import de.michab.scream.util.Continuation;
import de.michab.scream.util.Continuation.Cont;
import de.michab.scream.util.Continuation.Thunk;
import de.michab.scream.util.LoadContext;
import de.michab.scream.util.Scut;

/**
 * The scream script engine.
 *
 * @author Michael Binz
 */
public final class ScreamEvaluator implements ScriptEngine
{
    private static Logger LOG =
            Logger.getLogger( ScreamEvaluator.class.getName() );

    /**
     * The continuation processor.
     */
    private final Continuation<FirstClassObject> _continuation =
            new Continuation<>();

    /**
     * The relative path to our extensions package.
     */
    private final static String EXTENSION_POSITION = "extensions/";

    /**
     * The name of the file that holds the Scheme-implemented parts of the
     * engine common to all script engine instance.
     */
    private static final String schemeExtensions = "0-boot.s";

    /**
     * The interaction environment.
     * <p>
     * {@code r7rs 6.12 p55}
     */
    private final Environment _interaction;

    /**
     * The scheme-report environment.
     * <p>
     * {@code r7rs 6.12 p55}
     */
    private final Environment _schemeReport;

    private final Stack<ScriptContext> _context =
            new Stack<>();

    /**
     * The parent factory.
     */
    private final Scream _factory;

    /**
     * A reference to the per-thread evaluator that is used by the scream
     * runtime.
     */
    private final static ThreadLocal<ScreamEvaluator> _EVAL =
            new ThreadLocal<>();
    public final static ScreamEvaluator EVAL()
    {
        return Objects.requireNonNull( _EVAL.get() );
    }

    /**
     * Create a SchemeEvaluator.
     *
     * @param tle An environment used for evaluating the incoming expressions.
     */
    ScreamEvaluator(
            Scream interpreter )
        throws RuntimeX
    {
        _EVAL.set( this );

        _context.push( new SimpleScriptContext() );

        _factory =
                interpreter;
        _schemeReport =
                _topLevelEnvironment.extend( "tle-interpreter" );
        _schemeReport.define(
                Symbol.createObject( "scream:tle-interpreter" ),
                _schemeReport );

        // Load extensions defined in scheme source files.
        addExtensions(
        		_schemeReport,
        		schemeExtensions );
        FirstClassObject.setConstant(
                _schemeReport );
        _interaction = _schemeReport.extend(
                "interaction" );

        _interaction.define(
                Symbol.createObject( "scream:interaction" ),
                _interaction );
    }

    /**
     * Get the standard input port for this interpreter instance.
     *
     * @return This interpreter's standard in.
     */
    public PortIn getInPort()
    {
        return FirstClassObject.setConstant(
                new PortIn(
                        "stdin",
                        _context.peek().getReader() ) );
    }

    /**
     * Get the standard output port for this interpreter instance.
     *
     * @return This interpreter's standard out.
     */
    public PortOut getOutPort()
    {
        return FirstClassObject.setConstant(
                new PortOut(
                        "stdout",
                        _context.peek().getWriter() ) );
    }

    /**
     * Get the standard error port for this interpreter instance.
     *
     * @return This interpreter's standard error port.
     */
    public PortOut getErrorPort()
    {
        return FirstClassObject.setConstant(
                new PortOut(
                        "stderr",
                        _context.peek().getErrorWriter() ) );
    }

    /**
     * {@code r7rs 6.12 p55}
     *
     * @return the scheme-interaction-environment.
     */
    public Environment getInteraction()
    {
        return _interaction;
    }

    /**
     * {@code r7rs 6.12 p55}
     *
     * @return the scheme-report-environment.
     */
    public Environment getSchemeReport()
    {
        return _interaction;
    }

    /**
     * Evaluates the expressions read from the passed Reader in the Scream
     * type system.
     *
     * @param reader Delivers the expressions to be evaluated.
     * @return The evaluation result.
     * @throws RuntimeX In case of an error.
     */
    private FirstClassObject evalFco(Reader reader) throws RuntimeX
    {
        REGS.CENV( _interaction );

        return evalImpl(
                _interaction,
                new SchemeParser(
                        reader,
                        "ScreamEvaluator.evalFco(...)" )::getExpression );
    }

    /**
     * Evaluates the passed expressions in the Scream
     * type system.
     *
     * @param script The expressions to be evaluated.
     * @return The evaluation result.
     * @throws RuntimeX In case of an error.
     */
    public FirstClassObject evalFco(String script) throws RuntimeX {
        return evalFco( new StringReader( script ) );
    }

    private static Thunk evalImpl_(
            Environment e,
            SupplierX<FirstClassObject,RuntimeX> s,
            FirstClassObject previousResult,
            Cont<FirstClassObject> c )
                    throws RuntimeX
    {
        final var newExpression = s.get();

        if ( newExpression == Port.EOF )
            return c.accept( previousResult );

        return Primitives._eval(
                e,
                newExpression,
                fco -> _thunked_evalImpl_( e, s, fco, c ) );
    }

    private static Thunk _thunked_evalImpl_(
            Environment e,
            SupplierX<FirstClassObject,RuntimeX> s,
            FirstClassObject previousResult,
            Cont<FirstClassObject> c )
    {
        return () -> evalImpl_( e, s, previousResult, c );
    }

    private static Thunk evalImpl(
            Environment e,
            SupplierX<FirstClassObject,RuntimeX> s,
            Cont<FirstClassObject> c )
                    throws RuntimeX
    {
        return evalImpl_(
                e,
                s,
                Cons.NIL,
                c );
    }

    // Check if this is a good position.
    public static ThreadLocal<Continuation<FirstClassObject>> CONT =
            new ThreadLocal<>();

    private FirstClassObject evalImpl(
            Environment env,
            SupplierX<FirstClassObject,RuntimeX> spl )
                    throws RuntimeX
    {
        // Makes the continuation available in the interpreter.
        CONT.set( _continuation );

        try
        {
            return _continuation.toStack( c -> evalImpl( env, spl, c ) );
        }
        catch ( RuntimeX rx )
        {
            throw rx;
        }
        catch ( Exception x )
        {
            x.printStackTrace();
            throw Raise.mInternalError( x );
        }
    }

    /**
     * Loads the specified extensions from Scheme source files. These
     * have to be located in a special package de.michab.scream.extensions and
     * each file has to be specified in the Scream.properties file by the
     * kernel.schemeExtensions key.
     *
     * @param env The environment used for evaluating the extensions.
     * @param filename The files to load.
     */
    private void addExtensions(
            Environment env,
            String filename )
    {
        String crtFileName = EXTENSION_POSITION + filename;

        // Try to get a stream on the file...
        var url = Scream.class.getResource( crtFileName );

        JavaUtil.Assert(
                url != null,
                "File for processing not found: '%s'",
                crtFileName );

        try
        {
            load( url, env );
        }
        catch ( RuntimeX e )
        {
            LOG.log(
                    Level.SEVERE,
                    e.getMessage() );
            throw new InternalError( e );
        }
    }

    /**
     * Loads a Scheme source file into the passed environment.
     *
     * @param filename The name of the file to load.
     * @throws RuntimeX In case of errors.
     */
    public FirstClassObject load( SchemeString filename, Environment environment )
            throws RuntimeX
    {
        return load( new LoadContext( filename.getValue() ), environment );
    }

    /**
     * Loads a Scheme source file into the passed environment.
     *
     * @param filename The URL of the file to load.
     * @throws RuntimeX In case of errors.
     */
    private FirstClassObject load( URL filename, Environment environment )
            throws RuntimeX
    {
        return load( new LoadContext( filename ), environment );
    }

    /**
     * Load a Scheme source file.
     *
     * @param file The name of the file to load.
     * @throws RuntimeX In case of errors.
     */
    private FirstClassObject load( LoadContext file, Environment e )
            throws RuntimeX
    {
        try ( var reader  = LoadContext.getReader( file ) )
        {
            SchemeParser parser =
                    new SchemeParser( reader, file.toString() );

            return evalImpl( e, parser::getExpression );
        }
        catch ( IOException ioe )
        {
            throw Raise.mIoError( ioe );
        }
    }

    /**
     * Apply an operation on a list of arguments.
     *
     * @param elementType The target type for the list elements.
     * @param operation The operation to apply.
     * @param e The environment for evaluation.
     * @param args The argument list.
     * @param previousResult The result of the previous application.
     * @param c The continuation receiving the result.
     * @return A thunk.
     * @throws RuntimeX
     */
    private static <T extends FirstClassObject>
    Thunk _apply(
            Class<T> elementType,
            FunctionX<T, FirstClassObject, RuntimeX> operation,
            Environment e,
            Cons args,
            FirstClassObject previousResult,
            Cont<FirstClassObject> c ) throws RuntimeX
    {
        if ( args == Cons.NIL )
            return c.accept( previousResult );


        Cont<FirstClassObject> next =
                (fco) -> _thunked_apply(
                        elementType,
                        operation,
                        e,
                        args.getCdr(),
                        fco,
                        c );

        return Primitives._eval(
                e,
                operation.apply( Scut.as( elementType, args.getCar() ) ),
                next );
    }

    private static <T extends FirstClassObject>
    Thunk _thunked_apply(
            Class<T> elementType,
            FunctionX<T, FirstClassObject, RuntimeX> operation,
            Environment e,
            FirstClassObject args,
            FirstClassObject previousResult,
            Cont<FirstClassObject> c )
    {
        return () -> _apply(
                elementType,
                operation,
                e,
                Scut.as( Cons.class, args ),
                previousResult,
                c );
    }

    /**
     * Apply an operation on a list of arguments.
     *
     * @param elementType The target type for the list elements.
     * @param operation The operation to apply.
     * @param e The environment for evaluation.
     * @param args The argument list.
     * @param c The continuation receiving the result.
     * @return A thunk.
     * @throws RuntimeX
     */
    private static <T extends FirstClassObject>
    Thunk _x_apply(
            Class<T> elementType,
            FunctionX<T, FirstClassObject, RuntimeX> operation,
            Environment e,
            Cons args,
            Cont<FirstClassObject> c ) throws RuntimeX
    {
        return () -> _apply(
                elementType,
                operation,
                e,
                args,
                Cons.NIL,
                c );
    }

    /**
     * {@code (include <string₁> <string₂> ...)}
     * <p>
     * {@code r7rs 4.1.7 p14} syntax
     */
    private final Syntax includeSyntax = new Syntax( "include" )
    {
        @Override
        protected Thunk _executeImpl( Environment e, Cons args, Cont<FirstClassObject> c )
                throws RuntimeX
        {
            checkArgumentCount( 1, Integer.MAX_VALUE, args );

            return _x_apply(
                    SchemeString.class,
                    s -> { return load( s, e ); },
                    e,
                    args,
                    c );
        }
    };

    /**
     * {@code (eval exp-or-def environment-specifier)}
     * <p>
     * {code r7rs 6.1.2 p55} eval library procedure
     */
    static private Procedure evalProcedure( Environment e )
    {
        return new Procedure( "eval", e )
        {
            @Override
            protected Thunk _executeImpl(
                    Environment e,
                    Cons args,
                    Cont<FirstClassObject> c )
                            throws RuntimeX
            {
                checkArgumentCount( 2, args );

                return FirstClassObject.evaluate(
                        args.listRef( 0 ),
                        Scut.as(
                                Environment.class,
                                args.listRef( 1 ) ),
                        c );
            }
        };
    }

    /**
     * {@code (scream:eval <expression>)}
     * <p>
     * Evaluate an expression in the current environment.
     */
    private final Syntax evalSyntax = new Syntax( "scream:eval" )
    {
        @Override
        protected Thunk _executeImpl( Environment e, Cons args, Cont<FirstClassObject> c )
                throws RuntimeX
        {
            checkArgumentCount( 1, args );

            // (2) Evaluates the quoted expression.
            Cont<FirstClassObject> second = fco -> {
                return Primitives._eval( e, fco, c );
            };

            // (1) Evaluate the quote expression.
            return Primitives._eval( e, args.getCar(), second );
        }
    };

    /**
     * A reference to the top level environment.  This is a single common
     * instance holding all the core scheme definitions.  This instance is
     * shared between all interpreter instances and represents the root in
     * the environment hierarchy.
     * <p>
     * Assignment of values to symbols bound in this environment will take
     * place for all SchemeInterpreter instances.
     */
    private final Environment _topLevelEnvironment =
            createTle();

    /**
     * @return The newly allocated top level environment.
     */
    private Environment createTle()
    {
        var result = createNullEnvironment().extend( "tle-common" );

        try
        {
            PrimitiveProcedures.extendEnvironment( result );
            result.setPrimitive( evalProcedure( result ) );
            de.michab.scream.fcos.Continuation.extendTopLevelEnvironment( result );
            Number.extendTopLevelEnvironment( result );
            SchemeObject.extendTopLevelEnvironment( result );
            Exceptions_6_11.extendEnvironment( result );
        }
        catch ( Exception e )
        {
            LOG.log(
                    Level.SEVERE,
                    "Init threw exception.",
                    e.getCause() );
            throw new InternalError( e );
        }

        return FirstClassObject.setConstant( result );
    }

    /**
     * Creates the {@code null-environment}.
     * <p>
     * {@code r7rs 6.12 p55}
     *
     * @return the immutable {@code null-environment}.
     */
    private Environment createNullEnvironment()
    {
        Environment result = new Environment( "null" );

        try
        {
            SyntaxAnd.extendNullEnvironment( result );
            SyntaxAssign.extendNullEnvironment( result );
            SyntaxBegin.extendNullEnvironment( result );
            SyntaxCase.extendNullEnvironment( result );
            SyntaxCond.extendNullEnvironment( result );
            SyntaxDefine.extendNullEnvironment( result );
            SyntaxDefineValues.extendNullEnvironment( result );
            SyntaxDo.extendNullEnvironment( result );
            SyntaxIf.extendNullEnvironment( result );
            SyntaxLambda.extendNullEnvironment( result );
            result.setPrimitive( SyntaxLet.letAsteriskSyntax );
            result.setPrimitive( SyntaxLet.letSyntax );
            result.setPrimitive( SyntaxLet.letrecSyntax );
            result.setPrimitive( SyntaxLetValues.letValuesSyntax );
            result.setPrimitive( SyntaxLetValues.letAsteriskValuesSyntax );
            SyntaxOr.extendNullEnvironment( result );
            SyntaxQuote.extendNullEnvironment( result );
            SyntaxSyntax.extendNullEnvironment( result );
            SyntaxTime.extendNullEnvironment( result );

            result.setPrimitive( evalSyntax );
            result.setPrimitive( includeSyntax );

            result.define(
                    Symbol.createObject( "scream:null-environment" ),
                    result );

            return FirstClassObject.setConstant( result );
        }
        catch ( Exception e )
        {
            throw new InternalError( e );
        }
    }

    //
    // ScriptEngine operations.
    //

    @Override
    public Object eval(String script, ScriptContext context) throws ScriptException
    {
        try
        {
            _context.push( context );
            return eval( script );
        }
        finally
        {
            _context.pop();
        }
    }

    @Override
    public Object eval(Reader reader, ScriptContext context) throws ScriptException {
        try
        {
            _context.push( context );
            return eval( reader );
        }
        finally
        {
            _context.pop();
        }
    }

    @Override
    public Object eval(String script) throws ScriptException {
        return eval( new StringReader( script ) );
    }

    @Override
    public Object eval(Reader reader) throws ScriptException
    {
        try
        {
            return FirstClassObject.toString( evalFco( reader ) );
        }
        catch ( RuntimeX e )
        {
            throw new ScriptException( e );
        }
    }

    @Override
    public Object eval(String script, Bindings n) throws ScriptException {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public Object eval(Reader reader, Bindings n) throws ScriptException {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public void put(String key, Object value) {
        // TODO Auto-generated method stub
    }

    @Override
    public Object get(String key) {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public Bindings getBindings(int scope) {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public void setBindings(Bindings bindings, int scope) {
        // TODO Auto-generated method stub

    }

    @Override
    public Bindings createBindings() {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public ScriptContext getContext()
    {
        return _context.peek();
    }

    @Override
    public void setContext( ScriptContext context )
    {
        Objects.requireNonNull( context );

        if ( null == context.getErrorWriter() )
            context.setErrorWriter( Writer.nullWriter() );
        if ( null == context.getReader() )
            context.setReader( Reader.nullReader() );
        if ( null == context.getWriter() )
            context.setErrorWriter( Writer.nullWriter() );

        _context.clear();
        _context.push( context );
    }

    @Override
    public ScriptEngineFactory getFactory()
    {
        return _factory;
    }
}

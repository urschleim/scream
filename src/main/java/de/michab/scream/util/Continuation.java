/*
 * Scream @ https://github.com/urschleim/scream
 *
 * Copyright © 2022-2024 Michael G. Binz
 */
package de.michab.scream.util;

import java.util.ArrayDeque;
import java.util.Deque;
import java.util.Objects;

import org.smack.util.Holder;
import org.smack.util.FunctionalUtil.SupplierX;

import de.michab.scream.RuntimeX;

/**
 * Trampoline-based continuation-passing infrastructure used to evaluate
 * Scheme expressions without growing the Java call stack, and to
 * implement Scheme's {@code call-with-current-continuation} and
 * {@code with-exception-handler}/{@code raise} (r7rs 6.10, 6.11).
 * <p>
 * Computations are expressed as chains of {@link Thunk}s. {@link #toStack}
 * repeatedly runs a thunk chain to completion ({@link #trampoline}) and
 * returns the final result of type {@code T}. {@link RuntimeX}s that occur
 * while a chain is running are not propagated natively; instead they are
 * redirected to whichever {@link Cont} is currently on top of the handler
 * stack maintained by {@link #pushExceptionHandler} /
 * {@link #popExceptionHandler}, which is how
 * {@code with-exception-handler} is implemented. Pushes and pops of that
 * handler stack must be balanced by the caller; an unmatched pop fails
 * fast (see {@link #popExceptionHandler}). All other, unchecked,
 * exceptions propagate natively.
 * <p>
 * A single instance is designed to be reused across many, possibly
 * nested or sequential, {@link #toStack} calls (see {@code ScreamEvaluator}
 * use through {@code evalFco}/{@code load}). This is essential for
 * escaping ("full", non-delimited) continuations captured via
 * {@code call-with-current-continuation}: such a continuation can be
 * invoked from a later, unrelated {@code toStack()} call and must still
 * be able to deliver its result to whichever call is running at that
 * moment, which is why {@link #_result} is a shared instance field rather
 * than call-local state (see its Javadoc for the concrete failure mode of
 * "fixing" that). A consequence is that instances are <b>not
 * thread-safe</b> and must be confined to a single thread; concurrent use
 * of one instance from multiple threads is unsupported.
 *
 * @param <T> The overall result type produced by a completed thunk chain.
 *
 * @author micbinz
 */
public class Continuation<T>
{
    private long _thunkCount;

    /**
     * Will receive the result for all continuation invocations.
     * <p>
     * This MUST be a shared instance field and must NOT be replaced by a
     * variable local to {@link #toStack(ToStackOp, Cont)}. Scheme's
     * {@code call-with-current-continuation} can capture a continuation in
     * one {@code toStack()} call and invoke it later, from a completely
     * different, later {@code toStack()} call on this same instance (see
     * {@code de.michab.scream.fcos.Continuation}, and the test
     * {@code R7rs_6_10_Control_features_Test#call_cc_restart_2}). Invoking
     * such an escaped continuation discards the later call's own
     * continuation chain and resumes the original, captured chain instead,
     * which bottoms out in the exit continuation created inline by
     * {@link #toStack(ToStackOp, Cont)}, belonging to the original,
     * by-then-already-returned call. For that exit continuation to still
     * be able to deliver its result to whichever
     * {@code toStack()} call is currently running its trampoline, the slot
     * it writes to has to be shared across calls, not call-local. A
     * call-local {@code Holder} looks like a safer, more "correct"
     * alternative but silently breaks cross-call/escaping continuations.
     */
    private final Holder<T> _result = new Holder<>();

    @FunctionalInterface
    public static interface ToStackOp<T> {
        Thunk call( Cont<T> c )
            throws RuntimeX;
    }

    @FunctionalInterface
    public static interface Cont<R> {
        Thunk accept(R result) throws RuntimeX;
    }

    @FunctionalInterface
    public static interface Thunk {
        Thunk run() throws RuntimeX;
    }

    private Deque<Cont<RuntimeX>> _exceptionHandlers = new ArrayDeque<>();

    /**
     * Runs the passed thunk chain to completion, redirecting {@link RuntimeX}s
     * to the exception handler currently on top of {@link #_exceptionHandlers}.
     *
     * @param t The thunk to execute.
     * @throws RuntimeX If the exception handler invoked for a {@link RuntimeX}
     * itself throws.
     */
    private void trampoline(
            Thunk t )
                    throws RuntimeX
    {
        while ( t != null )
        {
            _thunkCount++;

            try
            {
                t = t.run();
            }
            catch ( RuntimeX rx )
            {
                // The passed exception continuation is called. If this in
                // turn throws an exception then thunk processing is
                // terminated.
                t = _exceptionHandlers.peek().accept( rx );
            }
        }
    }

    /**
     * @return The current value of the thunk counter. Used
     * in testing.
     */
    public long thunkCount()
    {
        return _thunkCount;
    }

    /**
     * Set the thunk counter.  Used in debugging.
     *
     * @param newValue The new value for the thunk counter.
     */
    public void thunkCount( long newValue )
    {
        _thunkCount = newValue;
    }

    /**
     * Execute a continuation-based operation with an exception handler and
     * return the result on the stack.
     *
     * @param <T> The result type.
     * @param op The operation to execute.
     * @param exceptionHandler The exception handler.  Must not
     * be {@code null}.
     * @return The result of the passed operation.
     * @throws RuntimeX
     */
    public
    T toStack(
            ToStackOp<T> op,
            Cont<RuntimeX> exceptionHandler )
        throws RuntimeX
    {
        Objects.requireNonNull( exceptionHandler );

        _result.set( null );

        try
        {
            pushExceptionHandler( exceptionHandler );

            // Note: op.call(...) itself runs here, outside of trampoline()'s
            // catch. A synchronous throw from it (before it returns a Thunk)
            // is therefore not redirected through the handler stack above,
            // unlike every later step of the resulting chain. This is
            // harmless today only because the handler just pushed above is
            // already the one that would apply. Do not "fix" this by routing
            // it through the same catch -- re-entering that catch for the
            // very exception it just caught would loop forever.
            trampoline(
                    op.call( s -> { _result.set( s ); return null; } ) );

            return _result.get();
        }
        finally
        {
            popExceptionHandler();
        }
    }

    /**
     * Execute a continuation-based operation and
     * return the result on the stack.
     *
     * @param <T> The result type.
     * @param op The operation to execute.
     * @return The result of the passed operation.
     * @throws RuntimeX
     */
    public
    T toStack( ToStackOp<T> op )
        throws RuntimeX
    {
        Holder<RuntimeX> exception = new Holder<>();

        Cont<RuntimeX> handler =
                ae -> {
                    exception.set( ae );
                    return null;
                };

        toStack( op, handler );

        if ( exception.get() != null )
            throw exception.get();

        return _result.get();
    }

    /**
     * Adapts a {@link SupplierX} typed to {@link RuntimeX} into a
     * {@link Thunk}.
     *
     * @param <R> The result type returned by the supplier.
     * @param supplier Supplies the value passed to {@code c}.
     * @param c Receives the supplied value.
     * @return A thunk.
     */
    public <R> Thunk toCont( SupplierX<R,RuntimeX> supplier, Cont<R> c )
    {
        return () -> c.accept( supplier.get() );
    }

    public void pushExceptionHandler( Cont<RuntimeX> handler )
    {
        _exceptionHandlers.push(
                Objects.requireNonNull( handler ) );
    }

    /**
     * Removes and returns the exception handler currently on top of the
     * handler stack.
     *
     * @return The removed handler.
     * @throws IllegalStateException If called without a matching prior
     * {@link #pushExceptionHandler(Cont)}, i.e. the handler stack is empty.
     * This guards against a confusing raw {@code NoSuchElementException}
     * from an unbalanced push/pop in caller code.
     */
    public Cont<RuntimeX> popExceptionHandler()
    {
        if ( _exceptionHandlers.isEmpty() )
            throw new IllegalStateException(
                    "popExceptionHandler() called without a matching pushExceptionHandler()." );

        return _exceptionHandlers.pop();
    }
}

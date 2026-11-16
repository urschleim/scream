/*
 * Scream @ https://github.com/urschleim/scream
 *
 * Copyright © 2022-2023 Michael G. Binz
 */

package de.michab.scream.util;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.Reader;
import java.net.URI;
import java.net.URL;
import java.util.Stack;

/**
 * Support for file loading.  Implements the necessary logic to load files from
 * the filesystem, inside a jar file or from the internet.  Supports loading of
 * several files that are rooted to a single source.  That means if the first
 * file is rooted in a jar file then follow-up files with a relative name are
 * loaded from the same source.
 * <p>
 * See // https://flylib.com/books/en/1.134.1/jarurlconnection.html for the
 * basic idea.
 *
 * @author micbinz
 */
public class LoadContext
{
    private final String _prefix;
    private final File _file;
    private final boolean _isFile;

    private final static String FILE_PROTOCOL = "file";
    private final static String JAR_PROTOCOL = "jar";
    private final static String EXCLAMATION = "!";

    private static URL nonThrowingUrl( String name )
    {
        try
        {
            return new URI( name ).toURL();
        }
        catch ( Exception ignore )
        {
        }

        try
        {
            return new URI( FILE_PROTOCOL + ":" + name.replace( '\\', '/' ) ).toURL();
        }
        catch ( Exception ignore )
        {
        }

        throw new IllegalArgumentException( name );
    }

    public LoadContext( String filename ) {
        this( nonThrowingUrl( filename ) );
    }

    public LoadContext( URL filename )
    {
        if ( FILE_PROTOCOL.equals( filename.getProtocol() ) )
        {
            _prefix = FILE_PROTOCOL + ":";
            _file = new File( filename.getPath() );
            _isFile = true;
            return;
        }
        if ( JAR_PROTOCOL.equals( filename.getProtocol() ) )
        {
            var split = filename.toExternalForm().split( EXCLAMATION );
            _prefix = split[0] + EXCLAMATION;
            _file = new File( split[1] );
            _isFile = false;
            return;
        }

        throw new IllegalArgumentException( filename.toExternalForm() );
    }

    private LoadContext( String prefixx, File file, boolean isFile )
    {
        _prefix = prefixx;
        _file = file;
        _isFile = isFile;
    }

    File getFile()
    {
        return _file;
    }

    public boolean isAbsolute()
    {
        return _file.getPath().startsWith( File.separator );
    }

    private boolean hasParent()
    {
        return _file.getParent() != null;
    }

    LoadContext relate( LoadContext prev )
    {
        var f = isAbsolute() ?
                getFile() :
                new File( prev._file.getParentFile(), _file.getName() );

        return new LoadContext(
                prev._prefix,
                f,
                prev._isFile );
    }

    private InputStream getStream() throws IOException
    {
        if ( _isFile )
            return new FileInputStream( _file );

        return toUrl().openStream();
    }

    private URL toUrl()
    {
        var asString =
                _prefix + _file;
        return nonThrowingUrl(
                asString.replace( '\\', '/' ) );
    }

    @Override
    public String toString()
    {
        return toUrl().toExternalForm();
    }

    /**
     * @return {@code true} if this represents a file, otherwise
     * this is an URL.
     */
    public boolean isFile()
    {
        return _isFile;
    }

    private final static ThreadLocal<Stack<LoadContext>> loadStack =
            ThreadLocal.withInitial( Stack<LoadContext>::new );


    public static Reader getReader( LoadContext current )
            throws IOException
    {
        var stack = loadStack.get();

        if ( stack.isEmpty() )
            ;
        else if ( current.isAbsolute() )
            ;
        else if ( ! stack.peek().hasParent() )
            ;
        else
            current = current.relate( stack.peek() );

        stack.push( current );

        return new InputStreamReader( current.getStream() ) {
            @Override
            public void close() throws IOException
            {
                stack.pop();
                super.close();
            }
        };
    }
}

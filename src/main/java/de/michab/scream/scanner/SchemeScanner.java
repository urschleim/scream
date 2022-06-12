/* $Id: SchemeScanner.java 1 2008-09-19 16:30:02Z binzm $
 *
 * Project: Scream / Frontend
 *
 * Released under Gnu Public License
 * Copyright (c) 1998-2000 Michael G. Binz
 */
package de.michab.scream.scanner;

import java.io.IOException;
import java.io.Reader;




/**
 * The scanner implementation.  Works as one would expect.  Text from the input
 * is broken into tokens.  Delegates its responsibilities to some automatically
 * generated scanner.
 */
public class SchemeScanner
{
  /**
   * This is the real scanner generated by JFlex.
   */
  private SchemeFlexScanner _scanner;



  /**
   * Create a new scanner on the passed <code>Reader</code>.
   */
  public SchemeScanner( Reader source )
  {
    _scanner = new SchemeFlexScanner( source );
  }



  /**
   * Get the next token from the input stream.
   */
  public Token getNextToken()
    throws
      FrontendX
  {
    // No scanner means that we reached the end of input.
    if ( _scanner == null )
      return Token.createToken( SchemeParser.TkEof );

    try
    {
      Token token = _scanner.getNextToken();

      if ( token.getType() == SchemeParser.TkEof )
      {
          // If we reached EOF, we throw away the scanner...
          _scanner = null;
          // ...before doing business as usual.
      }

      return token;
    }
    catch ( IOException e )
    {
      throw new FrontendX( "IO_ERROR",
                            new Object[]{ e.getMessage() } );
    }
  }
}

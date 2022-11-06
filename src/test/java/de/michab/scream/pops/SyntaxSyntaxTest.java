package de.michab.scream.pops;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;

import org.junit.jupiter.api.Test;

import de.michab.scream.Operation;
import de.michab.scream.ScreamBaseTest;
import de.michab.scream.ScreamEvaluator;

public class SyntaxSyntaxTest extends ScreamBaseTest
{
    @Test
    public void exists() throws Exception
    {
        ScreamEvaluator se = scriptEngine();

        var result = se.evalFco(
                """
                %syntax
                """ );
        assertInstanceOf( Operation.class, result );
    }


    @Test
    public void syntaxSyntaxTest() throws Exception
    {
        var se = scriptEngine();

        var result = se.eval(
                """
                (%syntax (xquote value) value)
                (xquote micbinz)
                """ );
        assertEquals( "micbinz", result );
    }

}
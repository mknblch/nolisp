package de.mknblch.sucode.lexer;

import org.junit.Ignore;
import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import static org.junit.Assert.*;

/**
 * Created by pexx on 03.10.2014.
 */
public class LexerTest {

    static final Logger LOGGER = LoggerFactory.getLogger(LexerTest.class);

    @Test(expected = IllegalArgumentException.class)
    public void testNull() throws Exception {
        final Lexer lexer = new Lexer(null);
    }

    @Test
    public void testEmpty() throws Exception {
        final Lexer lexer = new Lexer("     ");
        assertFalse(lexer.hasNext());
    }

    @Test
    public void testCodeWithSpaces() throws Exception {
        final String code = " (sugar 1(+ 23 345)) ";
        final Lexer lexer = new Lexer(code);
        assertTokenEquals(new String[]{"(", "sugar", "1", "(", "+", "23", "345", ")", ")" }, lexer);
    }

    @Test
    public void testCodeWithTabs() throws Exception {
        final String code = " (sugar 1     (+      23 345) ) ";
        final Lexer lexer = new Lexer(code);
        assertTokenEquals(new String[]{"(", "sugar", "1", "(", "+", "23", "345", ")", ")" }, lexer);
    }
    @Test
    public void testCodeWithNewline() throws Exception {
        final String code = " (sugar\n1\n (+\n 23 345))";
        final Lexer lexer = new Lexer(code);
        assertTokenEquals(new String[]{"(", "sugar", "1", "(", "+", "23", "345", ")", ")" }, lexer);
    }

    @Test
    public void testQuote() throws Exception {
        final String code = "'(+ a b)";
        final Lexer lexer = new Lexer(code);
        assertTokenEquals(new String[]{"'", "(", "+", "a", "b", ")"}, lexer);
    }

    @Test
    public void testString() throws Exception {
        final String code = "\"abc\"";
        final Lexer lexer = new Lexer(code);
        assertTokenEquals(new String[]{"\"abc\""}, lexer);
    }

    @Test
    public void testEmptyString() throws Exception {
        final String code = "\"\"";
        final Lexer lexer = new Lexer(code);
        assertTokenEquals(new String[]{"\"\""}, lexer);
    }

    @Test(expected = LexerException.class)
    public void testUnbalancedString() throws Exception {
        final String code = " \"abc\" \" ";
        final Lexer lexer = new Lexer(code);

        while (lexer.hasNext()) {
            lexer.next();
        }
    }

    @Test
    public void testNegativeInteger() throws Exception {
        final String code = "-5";
        final Lexer lexer = new Lexer(code);
        assertTokenEquals(new String[]{"-5"}, lexer);
    }

    @Test
    public void testNegativeFloat() throws Exception {
        final String code = "-3.1415";
        final Lexer lexer = new Lexer(code);
        assertTokenEquals(new String[]{"-3.1415"}, lexer);
    }

    @Test
    public void testNegativeFloatCode() throws Exception {
        final String code = "(- -3.1415 -1)";
        final Lexer lexer = new Lexer(code);
        assertTokenEquals(new String[]{"(", "-", "-3.1415", "-1", ")"}, lexer);
    }


    @Test
    public void testLineComment() throws Exception {
        final String code = "abc ;; hallo welt\n123";
        final Lexer lexer = new Lexer(code);
        assertTokenEquals(new String[]{"abc", ";; hallo welt", "123"}, lexer);
    }

    @Ignore
    @Test
    public void testAsList() throws Exception {
        final String code = "(a b c)";
        final Lexer lexer = new Lexer(code);
        final String[] result = lexer.asList().toArray(new String[]{});
        assertArrayEquals(new String[]{"(", "a", "b", "c", ")"}, result);
    }

    /**
     * compare lexer output with expected values
     * @param expected
     * @param lexer
     * @throws LexerException
     */
    private static void assertTokenEquals (String[] expected, Lexer lexer) throws LexerException {
        for (int i = 0; i < expected.length; i++) {

            assertTrue("premature end at token " + i, lexer.hasNext());
            assertEquals("token " + i + " did not match", expected[i], lexer.next().literal);
        }
        assertFalse("lexer has more tokens", lexer.hasNext());
    }
}

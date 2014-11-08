package de.mknblch.nolisp.parser.lexer;

import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import static org.junit.Assert.*;

/**
 * @author mknblch
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
    public void testNIL() throws Exception {
        final String code = "NIL";
        final Lexer lexer = new Lexer(code);
        assertTokenEquals(new String[]{"nil"}, lexer);
    }

    @Test
    public void testTrue() throws Exception {
        final String code = "T";
        final Lexer lexer = new Lexer(code);
        assertTokenEquals(new String[]{"t"}, lexer);
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

    private List<Token> asList(Lexer lexer) throws LexerException {
        final ArrayList<Token> codeList = new ArrayList<Token>();
        while (lexer.hasNext()) {
            codeList.add(lexer.next());
        }
        return Collections.unmodifiableList(codeList);
    }

    /**
     * compare lexer output with expected values
     * @param expected
     * @param lexer
     * @throws LexerException
     */
    private static void assertTokenEquals (String[] expected, Lexer lexer) throws LexerException {
        LOGGER.debug("comparing: {}", merge(expected));
        for (int i = 0; i < expected.length; i++) {

            assertTrue("premature end at token " + i, lexer.hasNext());
            assertEquals("token " + i + " did not match", expected[i], lexer.next().literal);
        }
        assertFalse("lexer has more tokens", lexer.hasNext());
    }

    private static String merge (String[] strings) {
        final StringBuffer stringBuffer = new StringBuffer();
        for (int i = 0; i < strings.length; i++) {
            if(stringBuffer.length() > 0) stringBuffer.append(" ");
            stringBuffer.append(strings[i]);
        }
        return String.format("%s", stringBuffer.toString());
    }
}

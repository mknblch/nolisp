package de.mknblch.nolisp.parser.lexer;

import org.junit.Assert;
import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.math.BigDecimal;
import java.math.BigInteger;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

/**
 * @author mknblch
 */
public class LexerTest {

    static final Logger LOGGER = LoggerFactory.getLogger(LexerTest.class);

    @Test(expected = IllegalArgumentException.class)
    public void testNull() throws Exception {
        final Lexer lexer = new Lexer();
        lexer.setCode(null);
    }

    @Test
    public void testEmpty() throws Exception {
        final Lexer lexer = new Lexer();
        lexer.setCode("     ");
        Assert.assertFalse(lexer.hasNext());
    }

    @Test
    public void testCodeWithSpaces() throws Exception {
        final String code = " (sugar 1(+ 23 345)) ";
        final Lexer lexer = new Lexer();
        lexer.setCode(code);
        assertTokenEquals(new String[]{"(", "sugar", "1", "(", "+", "23", "345", ")", ")"}, lexer);
    }

    @Test(expected = NumberFormatException.class)
    public void testIntOverflow() throws Exception {
        final String code = "10000000000";
        final Lexer lexer = new Lexer();
        lexer.setCode(code);
        lexer.next();
    }

    @Test
    public void testLong() throws Exception {
        final String code = "10000000000L";
        final Lexer lexer = new Lexer();
        lexer.setCode(code);
        final Token next = lexer.next();
        assertTrue(next.value instanceof Long);
    }

    @Test
    public void testHexadecimal() throws Exception {
        final String code = "0x2A";
        final Lexer lexer = new Lexer();
        lexer.setCode(code);
        final Token next = lexer.next();
        assertTrue(next.value == 42);
    }

    @Test
    public void testCodeWithTabs() throws Exception {
        final String code = " (sugar 1     (+      23 345) ) ";
        final Lexer lexer = new Lexer();
        lexer.setCode(code);
        assertTokenEquals(new String[]{"(", "sugar", "1", "(", "+", "23", "345", ")", ")"}, lexer);
    }

    @Test
    public void testCodeWithNewline() throws Exception {
        final String code = " (sugar\n1\n (+\n 23 345))";
        final Lexer lexer = new Lexer();
        lexer.setCode(code);
        assertTokenEquals(new String[]{"(", "sugar", "1", "(", "+", "23", "345", ")", ")"}, lexer);
    }

    @Test
    public void testQuote() throws Exception {
        final String code = "'(+ a b)";
        final Lexer lexer = new Lexer();
        lexer.setCode(code);
        assertTokenEquals(new String[]{"quote", "(", "+", "a", "b", ")"}, lexer);
    }

    @Test
    public void testString() throws Exception {
        final String code = " \"abc\" ";
        final Lexer lexer = new Lexer();
        lexer.setCode(code);
        assertTokenEquals(new String[]{"abc"}, lexer);
    }

    @Test
    public void testEscapedString() throws Exception {
        final String code = "\"a\nb\\\"c\"";
        final Lexer lexer = new Lexer();
        lexer.setCode(code);
        assertTokenEquals(new String[]{"a\nb\"c"}, lexer);
    }

    @Test
    public void testEmptyString() throws Exception {
        final String code = "\"\"";
        final Lexer lexer = new Lexer();
        lexer.setCode(code);
        assertTokenEquals(new String[]{""}, lexer);
    }

    @Test(expected = LexerException.class)
    public void testUnbalancedString() throws Exception {
        final String code = " \"abc\" \" ";
        final Lexer lexer = new Lexer();
        lexer.setCode(code);

        while (lexer.hasNext()) {
            lexer.next();
        }
    }

    @Test
    public void testNegativeInteger() throws Exception {
        final String code = "-5";
        final Lexer lexer = new Lexer();
        lexer.setCode(code);
        assertTokenEquals(new String[]{"-5"}, lexer);
    }

    @Test
    public void testNegativeFloat() throws Exception {
        final String code = "-3.1415";
        final Lexer lexer = new Lexer();
        lexer.setCode(code);
        assertTokenEquals(new String[]{"-3.1415"}, lexer);
    }

    @Test
    public void testNIL() throws Exception {
        final String code = "NIL";
        final Lexer lexer = new Lexer();
        lexer.setCode(code);
        assertTokenEquals(new String[]{"nil"}, lexer);
    }

    @Test
    public void testJavaType() throws Exception {
        final String code = "int boolean";
        final Lexer lexer = new Lexer();
        lexer.setCode(code);
        assertTokenEquals(new String[]{"int", "boolean"}, lexer);
    }

    @Test
    public void testNegativeFloatCode() throws Exception {
        final String code = "(- -3.1415 -1)";
        final Lexer lexer = new Lexer();
        lexer.setCode(code);
        assertTokenEquals(new String[]{"(", "-", "-3.1415", "-1", ")"}, lexer);
    }

    @Test
    public void testLineComment() throws Exception {
        final String code = "abc ; hallo welt\n123";
        final Lexer lexer = new Lexer();
        lexer.setCode(code);
        assertTokenEquals(new String[]{"abc", "; hallo welt", "123"}, lexer);
    }

    @Test
    public void testBigInteger() throws Exception {
        final String code = "13333333333333333333333333333333333333333333337B";
        final Lexer lexer = new Lexer();
        lexer.setCode(code);
        final Token next = lexer.next();
        assertTrue(next.value instanceof BigInteger);
    }

    @Test
    public void testBigDecimalFPPart() throws Exception {
        final String code = "13.37d";
        final Lexer lexer = new Lexer();
        lexer.setCode(code);
        final Token next = lexer.next();
        assertTrue(next.value instanceof BigDecimal);
    }

    @Test
    public void testBigDecimal() throws Exception {
        final String code = "1337d";
        final Lexer lexer = new Lexer();
        lexer.setCode(code);
        final Token next = lexer.next();
        assertTrue(next.value instanceof BigDecimal);
    }

    @Test
    public void testLineCommentAtEnd() throws Exception {
        final String code = ";hallo";
        final Lexer lexer = new Lexer();
        lexer.setCode(code);
        assertTokenEquals(new String[]{";hallo"}, lexer);
    }

    /**
     * compare lexer output with expected values
     *
     * @param expected
     * @param lexer
     * @throws LexerException
     */
    private static void assertTokenEquals(String[] expected, Lexer lexer) throws LexerException {
        for (int i = 0; i < expected.length; i++) {

            assertTrue("premature end at token " + i, lexer.hasNext());
            final String literal = lexer.next().literal;
            LOGGER.debug("comparing expected '{}' with result '{}'", merge(expected), literal);
            assertEquals("token " + i + " did not match", expected[i], literal);
        }
        Assert.assertFalse("lexer has more tokens", lexer.hasNext());
    }

    private static String merge(String[] strings) {
        final StringBuffer stringBuffer = new StringBuffer();
        for (int i = 0; i < strings.length; i++) {
            if (stringBuffer.length() > 0) stringBuffer.append(" ");
            stringBuffer.append(strings[i]);
        }
        return String.format("%s", stringBuffer.toString());
    }
}

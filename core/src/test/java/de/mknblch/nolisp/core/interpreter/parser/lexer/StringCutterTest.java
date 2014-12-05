package de.mknblch.nolisp.core.interpreter.parser.lexer;

import org.junit.Assert;
import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * @author mknblch
 */
public class StringCutterTest {

    private static final Logger LOGGER = LoggerFactory.getLogger(StringCutterTest.class);
    public static final char[] SPACE = new char[]{' '};

    @Test
    public void testRunner() throws Exception {
        final StringCutter stringCutter = new StringCutter().setString("a ab 1 23");
        stringCutter.until(SPACE);
        assertToken(stringCutter, "a");
        stringCutter.skip(SPACE);
        stringCutter.sync();

        stringCutter.until(SPACE);
        assertToken(stringCutter, "ab");
        stringCutter.skip(SPACE);
        stringCutter.sync();

        stringCutter.until(SPACE);
        assertToken(stringCutter, "1");
        stringCutter.skip(SPACE);
        stringCutter.sync();

        stringCutter.until(SPACE);
        assertToken(stringCutter, "23");
        stringCutter.skip(SPACE);
        stringCutter.sync();

    }

    public void assertToken(StringCutter stringCutter, String expected) {
        LOGGER.debug("'{}'", stringCutter.getLiteral());
        Assert.assertEquals(expected, stringCutter.getLiteral());
    }

}

package de.mknblch.nolisp.core.parser.lexer;

import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import static org.junit.Assert.assertEquals;

/**
 * @author mknblch
 */
public class StringCutterTest {

    private static final Logger LOGGER = LoggerFactory.getLogger(StringCutterTest.class);
    
    @Test
    public void testRunner() throws Exception {
        final StringCutter stringCutter = new StringCutter("a ab 1 23");
        stringCutter.until(new char[]{' '});
        assertToken(stringCutter, "a");
        stringCutter.skip(new char[]{' '});
        stringCutter.sync();

        stringCutter.until(new char[]{' '});
        assertToken(stringCutter, "ab");
        stringCutter.skip(new char[]{' '});
        stringCutter.sync();

        stringCutter.until(new char[]{' '});
        assertToken(stringCutter, "1");
        stringCutter.skip(new char[]{' '});
        stringCutter.sync();

        stringCutter.until(new char[]{' '});
        assertToken(stringCutter, "23");
        stringCutter.skip(new char[]{' '});
        stringCutter.sync();

    }

    public void assertToken(StringCutter stringCutter, String expected) {
        LOGGER.debug("'{}'", stringCutter.getToken());
        assertEquals(expected, stringCutter.getToken() );
    }

}

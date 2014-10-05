package de.mknblch.sucode.parser;

import de.mknblch.sucode.lexer.Lexer;
import de.mknblch.sucode.lexer.LexerException;
import de.mknblch.sucode.parser.structs.ListStruct;
import org.junit.Before;
import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;

/**
 * Created by pexx on 05.10.2014.
 */
public class ParserTest {

    static final Logger LOGGER = LoggerFactory.getLogger(ParserTest.class);

    public static final Parser PARSER = new Parser();

    @Before
    public void setUp() throws Exception {

    }

    @Test
    public void testSimpleAST() throws Exception {
        String code = "(+ 1 2 3)(+ 1 2 3)";
        assertASTEquals("( ( + 1 2 3 ) ( + 1 2 3 ) )", code);
    }

    @Test
    public void testQuoted() throws Exception {
        String code = "'1";
        assertASTEquals("( ( quote 1 ) )", code);
    }

    private void dump(String code) throws LexerException, ParserException {
        System.out.println(FormatHelper.formatPretty(PARSER.parse(new Lexer(code).asList())));
    }

    private void assertASTEquals(String expected, String code) throws LexerException, ParserException {

        LOGGER.debug("Code    : {}", code);

        ListStruct parse = PARSER.parse(new Lexer(code).asList());

        String pretty = FormatHelper.formatPretty(parse);
        LOGGER.debug("AST     : {}", pretty);

        assertEquals(expected, pretty);
    }

}

package de.mknblch.sucode.parser;

import de.mknblch.sucode.lexer.Lexer;
import de.mknblch.sucode.lexer.LexerException;
import de.mknblch.sucode.lexer.Token;
import de.mknblch.sucode.parser.structs.ListStruct;
import org.junit.Ignore;
import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;

/**
 * Created by mknblch on 05.10.2014.
 */
public class StackParserTest {

    static final Logger LOGGER = LoggerFactory.getLogger(StackParserTest.class);

    private static final StackParser PARSER = new StackParser();

    @Test
    public void testSimpleAST() throws Exception {
        String code = "(+ 1 2 3)(+ 1 2 3)";
        assertASTEquals("( ( + 1 2 3 ) ( + 1 2 3 ) )", code);
    }

    @Test
    public void testQuotedInt() throws Exception {
        String code = "'1 x";
        assertASTEquals("( ( quote 1 ) x )", code);
    }


    @Test
    public void testQuotedSymbol() throws Exception {
        String code = "'x x";
        assertASTEquals("( ( quote x ) x )", code);
    }


    @Test
    public void testQuotedReal() throws Exception {
        String code = "'3.1415 3";
        assertASTEquals("( ( quote 3.1415 ) 3 )", code);
    }

    @Ignore
    @Test
    public void testQuotedList() throws Exception {
        String code = "'(1 2 3) x";
        assertASTEquals("( ( quote ( 1 2 3 ) ) x )", code);
    }

    @Ignore // not yet implemented
    @Test
    public void testDoubleQuoted() throws Exception {
        String code = "''1 2 3";
        assertASTEquals("( ( quote ( quote 1 ) ) 2 3 )", code);
    }

    @Ignore
    @Test
    public void testQuotedTwoLists() throws Exception {
        String code = "'(1 2 3)'(1 2 3)x";
        assertASTEquals("( ( quote ( 1 2 3 ) ) ( quote ( 1 2 3 ) ) x )", code);
    }

    @Test(expected = ParserException.class)
    public void shouldFail_UnbalancedBraceOpen() throws Exception {
        String code = "(()";
        ListStruct parse = parse(code);
    }

    @Test(expected = ParserException.class)
    public void shouldFail_UnbalancedBraceClose() throws Exception {
        String code = "())";
        ListStruct parse = parse(code);
    }

    @Test(expected = ParserException.class)
    public void testUnbalancedBraces() throws Exception {
        String code = ")(";
        ListStruct parse = parse(code);
    }

    private List<Token> asList(Lexer lexer) throws LexerException {
        final ArrayList<Token> codeList = new ArrayList<Token>();
        while (lexer.hasNext()) {
            codeList.add(lexer.next());
        }
        return Collections.unmodifiableList(codeList);
    }

    private ListStruct parse(String code) throws ParserException, LexerException {
        return PARSER.parse(asList(new Lexer(code)));
    }

    private void assertASTEquals(String expected, String code) throws LexerException, ParserException {
        LOGGER.debug("Code : {}", code);
        ListStruct parse = parse(code);
        String pretty = FormatHelper.formatPretty(parse);
        LOGGER.debug("AST  : {}", pretty);
        assertEquals(expected, pretty);
    }

}

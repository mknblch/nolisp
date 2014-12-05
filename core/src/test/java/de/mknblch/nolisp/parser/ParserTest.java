package de.mknblch.nolisp.parser;

import de.mknblch.nolisp.common.FormatHelper;
import de.mknblch.nolisp.interpreter.EvaluationException;
import de.mknblch.nolisp.datatypes.ListStruct;
import org.junit.Assert;
import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * @author mknblch
 */
public class ParserTest {

    static final Logger LOGGER = LoggerFactory.getLogger(ParserTest.class);

    private static final Parser PARSER = new Parser();

    @Test
    public void testEmptyList() throws Exception {
        String code = "()";
        assertASTEquals("( ( nil ) )", code);
    }

    @Test
    public void testSimpleConst() throws Exception {
        String code = "(x)";
        assertASTEquals("( ( x ) )", code);
    }

    @Test
    public void testSimpleAST() throws Exception {
        String code = "(+ 1 2 3)(+ 1 2 3)";
        assertASTEquals("( ( + 1 2 3 ) ( + 1 2 3 ) )", code);
    }

    @Test
    public void testComment() throws Exception {
        String code = "(+ 1 2 3) ;(+ 1 2 3) \n\n\n(oO)";
        assertASTEquals("( ( + 1 2 3 ) ( oO ) )", code);
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

    @Test
    public void testBackQuotedList() throws Exception {
        String code = "`(1 2 3) x";
        assertASTEquals("( ( backquote ( 1 2 3 ) ) x )", code);
    }

    @Test(expected = EvaluationException.class)
    public void testSpliceError() throws Exception {
        String code = "(1 @(a @b) 3)";
        assertASTEquals("does not matter", code);
    }

    @Test
    public void testSplicedList() throws Exception {
        String code = "(1 @(a @(b)) 3)";
        assertASTEquals("( ( 1 a b 3 ) )", code);
    }

    @Test
    public void testBackQuotedSplicedList() throws Exception {
        String code = "`(1 @(a b) 3) x";
        assertASTEquals("( ( backquote ( 1 a b 3 ) ) x )", code);
    }

    @Test
    public void testBackQuotedCommaList() throws Exception {
        String code = "`(,1 2 3) x";
        assertASTEquals("( ( backquote ( ( comma 1 ) 2 3 ) ) x )", code);
    }

    @Test
    public void testBackQuotedSpliceList() throws Exception {
        String code = "`(,1 2 3) x";
        assertASTEquals("( ( backquote ( ( comma 1 ) 2 3 ) ) x )", code);
    }

    @Test
    public void testQuotedList() throws Exception {
        String code = "'(1 2 3) x";
        assertASTEquals("( ( quote ( 1 2 3 ) ) x )", code);
    }

    @Test
    public void testDoubleQuoted() throws Exception {
        String code = "''1 2 3";
        assertASTEquals("( ( quote ( quote 1 ) ) 2 3 )", code);
    }

    @Test
    public void testDoubleQuotedList() throws Exception {
        String code = "''(1 2 3)";
        assertASTEquals("( ( quote ( quote ( 1 2 3 ) ) ) )", code);
    }

    @Test
    public void testQuotedTwoLists() throws Exception {
        String code = "'(1 2 3)'(1 2 3)x";
        assertASTEquals("( ( quote ( 1 2 3 ) ) ( quote ( 1 2 3 ) ) x )", code);
    }

    @Test
    public void testNil() throws Exception {
        String code = "(nil)";
        assertASTEquals("( ( nil ) )", code);
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

    @Test
    public void testFunction() throws Exception {
        String code = "#'+";
        assertASTEquals("( ( function ( quote + ) ) )", code);
    }

    private ListStruct parse(String code) throws Exception {
        return PARSER.parse(code);
    }

    private void assertASTEquals(String expected, String code) throws Exception {
        LOGGER.debug("code : {}", code.replaceAll("[\r\n]", "\\\\n"));
        ListStruct parse = parse(code);
        String pretty = FormatHelper.formatPretty(parse);
        LOGGER.debug("AST  : {}", pretty);
        LOGGER.debug("sExp : {}", FormatHelper.formatAsSExpression(parse));
        Assert.assertEquals(expected, pretty);
    }

}

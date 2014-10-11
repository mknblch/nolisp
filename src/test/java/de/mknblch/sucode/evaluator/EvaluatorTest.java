package de.mknblch.sucode.evaluator;

import de.mknblch.sucode.lexer.Lexer;
import de.mknblch.sucode.lexer.LexerException;
import de.mknblch.sucode.parser.FormatHelper;
import de.mknblch.sucode.parser.Parser;
import de.mknblch.sucode.parser.ParserException;
import de.mknblch.sucode.parser.structs.ListStruct;
import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.List;

import static org.junit.Assert.assertEquals;

/**
 * Created by mknblch on 09.10.2014.
 */
public class EvaluatorTest {

    private static final Logger LOGGER = LoggerFactory.getLogger(EvaluatorTest.class);

    public static final Parser PARSER = new Parser();

    @Test
    public void testPrint() throws Exception {
        final String code = "(print (+ 1 1(+ 2 3)))";
        eval(code);
    }

    @Test
    public void testEvaluate() throws Exception {
        final String code = "(+ 1 1(+ 2 3))";
        final List<Object> evaluated = eval(code);
        dump(evaluated);
        assertEquals(7, evaluated.get(0));
    }

    @Test
    public void testQuote() throws Exception {
        final String code = "'a";
        final List<Object> evaluated = eval(code);
        dump(evaluated);
    }

    @Test
    public void testEnvironment() throws Exception {
        final String code = "(+ 2 x)";
        final Environment env = new Environment();
        env.put("x", 3);
        final List<Object> evaluated = eval(code, env);
        dump(evaluated);
        assertEquals(5, evaluated.get(0));
    }

    private void dump(List<Object> evaluated) throws ParserException {
        for (int i = 0; i < evaluated.size(); i++) {
            LOGGER.debug("{}", FormatHelper.formatAtom(evaluated.get(i)));
        }
    }

    private List<Object> eval(String code) throws ParserException, EvaluationException, LexerException {
        return eval(code, new Environment());
    }

    private List<Object> eval(String code, Environment environment) throws LexerException, ParserException, EvaluationException {
        final ListStruct program = PARSER.parse(new Lexer(code));
        LOGGER.debug("Code: {}", FormatHelper.formatPretty(program));
        LOGGER.debug("AST : {}", FormatHelper.formatAtom(program));
        return new Evaluator().evaluate(program, environment);
    }
}

package de.mknblch.sucode.evaluator;

import de.mknblch.sucode.lexer.Lexer;
import de.mknblch.sucode.lexer.LexerException;
import de.mknblch.sucode.parser.FormatHelper;
import de.mknblch.sucode.parser.Parser;
import de.mknblch.sucode.parser.ParserException;
import de.mknblch.sucode.parser.Program;
import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.List;

/**
 * Created by mknblch on 09.10.2014.
 */
public class EvaluatorTest {

    private static final Logger LOGGER = LoggerFactory.getLogger(EvaluatorTest.class);

    public static final Parser PARSER = new Parser();

    @Test
    public void testEvaluate() throws Exception {

        String code = "(+ 1 1(+ 2 3))";

        List<Object> evaluated = eval(code);

        dump(evaluated);
    }

    private void dump(List<Object> evaluated) {
        for (int i = 0; i < evaluated.size(); i++) {
            LOGGER.debug("{}", evaluated.get(i));
        }
    }

    private List<Object> eval(String code) throws LexerException, ParserException {
        final Program program = PARSER.parse(new Lexer(code));
        LOGGER.debug("Eval: {}", FormatHelper.formatPretty(program));
        return new Evaluator().evaluate(program);
    }
}

package de.mknblch.nolisp.minimal.testHelper;

import de.mknblch.nolisp.core.scanner.FunctionDefinitionException;
import de.mknblch.nolisp.core.interpreter.structs.ListStruct;
import de.mknblch.nolisp.core.interpreter.CoreInterpreter;
import de.mknblch.nolisp.core.interpreter.Interpreter;
import de.mknblch.nolisp.core.minimal.Minimal;
import de.mknblch.nolisp.core.interpreter.parser.Parser;
import de.mknblch.nolisp.core.interpreter.parser.ParserException;
import de.mknblch.nolisp.core.interpreter.parser.lexer.LexerException;
import de.mknblch.nolisp.core.common.FormatHelper;
import de.mknblch.nolisp.core.interpreter.Context;
import org.junit.Assert;
import org.junit.BeforeClass;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.ArrayList;
import java.util.List;

import static org.junit.Assert.assertEquals;

/**
 * @author mknblch
 */
public abstract class AbstractFormTest {

    private static final Logger LOGGER = LoggerFactory.getLogger(AbstractFormTest.class);
    private static final Parser PARSER = new Parser();
    protected static Interpreter loggingInterpreter;
    protected static Interpreter coreInterpreter;

    @BeforeClass
    public static void setUp() throws FunctionDefinitionException {
        loggingInterpreter = new LoggingInterpreter();
        coreInterpreter = new CoreInterpreter();
    }

    protected void dump(List<Object> evaluated) throws ParserException {
        for (int i = 0; i < evaluated.size(); i++) {
            LOGGER.debug("evaluates to {}", FormatHelper.formatPretty(evaluated.get(i)));
        }
    }

    protected List<Object> eval(String code) throws Exception {
        final Context context = new Context(new Minimal());
        return eval(code, loggingInterpreter, context);
    }

    protected List<Object> eval(String code, Interpreter interpreter, Context context) throws Exception {
        final ListStruct program = PARSER.parse(code);
        final ArrayList<Object> ret = new ArrayList<Object>();
        LOGGER.debug("evaluating: {}", FormatHelper.formatPretty(program));
        for (Object p : program) {
            final Object eval = interpreter.eval(p, context);
            LOGGER.debug("result: {}", FormatHelper.formatPretty(eval));
            ret.add(eval);
        }
        LOGGER.debug("Context: [ {} ]", FormatHelper.formatContext(context, false));
        return ret;
    }


    protected static void assertASTEquals(String expected, Object evaluated) throws LexerException, ParserException {
        String pretty = FormatHelper.formatPretty(evaluated);
        LOGGER.debug("AST : {}", pretty);
        Assert.assertEquals(expected, pretty);
    }
}

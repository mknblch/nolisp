package de.mknblch.sucode.testHelper;

import de.mknblch.sucode.builtin.*;
import de.mknblch.sucode.interpreter.Context;
import de.mknblch.sucode.interpreter.CoreInterpreter;
import de.mknblch.sucode.interpreter.Interpreter;
import de.mknblch.sucode.testHelper.LoggingInterpreter;
import de.mknblch.sucode.helper.FormatHelper;
import de.mknblch.sucode.parser.Parser;
import de.mknblch.sucode.parser.ParserException;
import de.mknblch.sucode.ast.ListStruct;
import de.mknblch.sucode.parser.lexer.LexerException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.ArrayList;
import java.util.List;

import static org.junit.Assert.assertEquals;

/**
 * @author mknblch
 */
public abstract class AbstractFormTest {

    private static final Logger LOGGER = LoggerFactory.getLogger(MathFormsTest.class);
    private static final Parser PARSER = new Parser();
    protected static final Interpreter LOGGING_INTERPRETER = new LoggingInterpreter();
    protected static final Interpreter CORE_INTERPRETER = new CoreInterpreter();

    public static final Class[] BUILTIN_FORMS = new Class[]{
            SpecialForms.class,
            ConditionForms.class,
            PredicateForms.class,
            MathForms.class,
            ConsoleForms.class};

    protected void dump(List<Object> evaluated) throws ParserException {
        for (int i = 0; i < evaluated.size(); i++) {
            LOGGER.debug("evaluates to {}", FormatHelper.formatPretty(evaluated.get(i)));
        }
    }

    protected List<Object> eval(String code) throws Exception {
        final Context context = new Context(BUILTIN_FORMS);
        return eval(code, LOGGING_INTERPRETER, context);
    }

    protected List<Object> eval(String code, Interpreter interpreter, Context context) throws Exception {
        final ListStruct program = PARSER.parse(code);
        final ArrayList<Object> ret = new ArrayList<Object>();
        LOGGER.debug("evaluating: {}", FormatHelper.formatPretty(program));
        for (Object p : program) {
            final Object eval = interpreter.eval(p, context);
            LOGGER.debug("eval: {}", FormatHelper.formatPretty(eval));
            ret.add(eval);
        }
        LOGGER.debug("Context: [ {} ]", FormatHelper.formatContext(context, false));
        return ret;
    }


    protected static void assertASTEquals(String expected, Object evaluated) throws LexerException, ParserException {
        String pretty = FormatHelper.formatPretty(evaluated);
        LOGGER.debug("AST  : {}", pretty);
        assertEquals(expected, pretty);
    }
}
package de.mknblch.sucode.func;

import de.mknblch.sucode.builtin.ConsoleForms;
import de.mknblch.sucode.builtin.MathForms;
import de.mknblch.sucode.builtin.MathFormsTest;
import de.mknblch.sucode.builtin.SpecialForms;
import de.mknblch.sucode.interpreter.Context;
import de.mknblch.sucode.interpreter.Interpreter;
import de.mknblch.sucode.interpreter.LoggingInterpreter;
import de.mknblch.sucode.lexer.Lexer;
import de.mknblch.sucode.helper.FormatHelper;
import de.mknblch.sucode.parser.Parser;
import de.mknblch.sucode.parser.ParserException;
import de.mknblch.sucode.ast.ListStruct;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.ArrayList;
import java.util.List;

/**
 * Created by mknblch on 12.10.2014.
 */
public abstract class AbstractFormTest {

    private static final Logger LOGGER = LoggerFactory.getLogger(MathFormsTest.class);
    private static final Parser PARSER = new Parser();
    public static final Interpreter INTERPRETER = new LoggingInterpreter();

    protected void dump(List<Object> evaluated) throws ParserException {
        for (int i = 0; i < evaluated.size(); i++) {
            LOGGER.debug("evaluates to {}", FormatHelper.formatAtom(evaluated.get(i)));
        }
    }

    protected List<Object> eval(String code) throws Exception {
        final Context context = new Context();
        context.defineAll(FunctionBuilder.build(SpecialForms.class, MathForms.class, ConsoleForms.class));
        return eval(code, context);
    }

    protected List<Object> eval(String code, Context context) throws Exception {
        final ListStruct program = PARSER.parse(new Lexer(code));
        final ArrayList<Object> ret = new ArrayList<Object>();
        LOGGER.debug("code: {}", FormatHelper.formatPretty(program));
        LOGGER.debug("AST : {}", FormatHelper.formatAtom(program));
        for (Object p : program) {
            final Object eval = INTERPRETER.eval(p, context);
            ret.add(eval);
            LOGGER.debug("eval: {}", FormatHelper.formatAtom(eval));
        }
        return ret;
    }
}

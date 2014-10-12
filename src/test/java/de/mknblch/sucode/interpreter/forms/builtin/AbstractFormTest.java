package de.mknblch.sucode.interpreter.forms.builtin;

import de.mknblch.sucode.interpreter.Environment;
import de.mknblch.sucode.interpreter.Interpreter;
import de.mknblch.sucode.interpreter.forms.FormRegister;
import de.mknblch.sucode.lexer.Lexer;
import de.mknblch.sucode.parser.FormatHelper;
import de.mknblch.sucode.parser.Parser;
import de.mknblch.sucode.parser.ParserException;
import de.mknblch.sucode.parser.structs.ListStruct;
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

    protected void dump(List<Object> evaluated) throws ParserException {
        for (int i = 0; i < evaluated.size(); i++) {
            LOGGER.debug("evaluates to {}", FormatHelper.formatAtom(evaluated.get(i)));
        }
    }

    protected List<Object> eval(String code) throws Exception {
        return eval(code, new Environment());
    }

    protected List<Object> eval(String code, Environment environment) throws Exception {
        final ListStruct program = PARSER.parse(new Lexer(code));
        final ArrayList<Object> ret = new ArrayList<Object>();
        final Interpreter interpreter = new Interpreter(new FormRegister(MathForms.class,SpecialForms.class,ConsoleForms.class));

        LOGGER.debug("code: {}", FormatHelper.formatPretty(program));
        LOGGER.debug("AST : {}", FormatHelper.formatAtom(program));
        for (Object p : program) {
            ret.add(interpreter.eval(p, environment));
        }
        return ret;
    }
}

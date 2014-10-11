package de.mknblch.sucode.interpreter.forms.builtin;

import de.mknblch.sucode.interpreter.Environment;
import de.mknblch.sucode.interpreter.EvaluationException;
import de.mknblch.sucode.interpreter.Interpreter;
import de.mknblch.sucode.interpreter.forms.Function;
import de.mknblch.sucode.parser.structs.ListStruct;

/**
 * Created by mknblch on 12.10.2014.
 */
public class SpecialForms {

    @Function(symbol = "quote")
    public static Object quote(ListStruct args, Environment env, Interpreter ip) throws EvaluationException {
        return args.cdr();
    }
}

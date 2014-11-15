package de.mknblch.nolisp.core.minimal;

import de.mknblch.nolisp.core.scanner.Define;
import de.mknblch.nolisp.core.scanner.Special;
import de.mknblch.nolisp.core.interpreter.structs.ListStruct;
import de.mknblch.nolisp.core.common.Expectations;
import de.mknblch.nolisp.core.common.Converter;
import de.mknblch.nolisp.core.interpreter.Interpreter;
import de.mknblch.nolisp.core.interpreter.Context;

/**
 * @author mknblch
 */
public class ConditionForms {

    @Special
    @Define(value = "if") // (if condition yes no)
    public static Object ifForm(Interpreter interpreter, Context context, ListStruct args) throws Exception {
        final boolean condition = Converter.asBoolean(interpreter.eval(args.car(), context));
        final Object trueBranch = args.cdar();
        final Object falseBranch = args.cddar();
        if (condition) return interpreter.eval(trueBranch, context);
        else return interpreter.eval(falseBranch, context);
    }

    @Special
    @Define(value = "cond")
    public static Object condForm(Interpreter interpreter, Context context, ListStruct args) throws Exception {
        for (Object arg : args) {
            Expectations.expectList(arg);
            final ListStruct pair = (ListStruct) arg;
            final Object condition = interpreter.eval(pair.car(), context);
            if (Converter.asBoolean(condition)) {
                return interpreter.eval(pair.cdar(), context);
            }
        }
        return null;
    }


}

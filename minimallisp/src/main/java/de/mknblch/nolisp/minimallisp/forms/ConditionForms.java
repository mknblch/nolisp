package de.mknblch.nolisp.minimallisp.forms;

import de.mknblch.nolisp.core.annotations.Define;
import de.mknblch.nolisp.core.annotations.Special;
import de.mknblch.nolisp.core.ast.ListStruct;
import de.mknblch.nolisp.core.helper.Expectations;
import de.mknblch.nolisp.core.helper.TypeHelper;
import de.mknblch.nolisp.core.interpreter.Interpreter;
import de.mknblch.nolisp.core.interpreter.Context;

/**
 * @author mknblch
 */
public class ConditionForms {

    @Special
    @Define(value = "if") // (if condition yes no)
    public static Object ifForm(Interpreter interpreter, Context context, ListStruct args) throws Exception {
        final boolean condition = TypeHelper.asBoolean(interpreter.eval(args.car(), context));
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
            if (TypeHelper.asBoolean(condition)) {
                return interpreter.eval(pair.cdar(), context);
            }
        }
        return null;
    }


}

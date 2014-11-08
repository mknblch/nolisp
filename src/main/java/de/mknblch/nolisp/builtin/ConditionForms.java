package de.mknblch.nolisp.builtin;

import de.mknblch.nolisp.ast.ListStruct;
import de.mknblch.nolisp.func.Define;
import de.mknblch.nolisp.func.Special;
import de.mknblch.nolisp.helper.Expectations;
import de.mknblch.nolisp.interpreter.Context;
import de.mknblch.nolisp.interpreter.Interpreter;

import static de.mknblch.nolisp.helper.TypeHelper.*;

/**
 * @author mknblch
 */
public class ConditionForms {

    /*
        (if condition yes no)
     */
    @Special
    @Define(value = "if")
    public static Object ifForm(Interpreter interpreter, Context context, ListStruct args) throws Exception {
        final boolean condition = asBoolean(interpreter.eval(args.car(), context));
        final Object trueBranch = args.cdar();
        final Object falseBranch = args.cddar();
        if(condition) return interpreter.eval(trueBranch, context);
        else return interpreter.eval(falseBranch, context);
    }

    @Special
    @Define(value = "cond")
    public static Object condForm(Interpreter interpreter, Context context, ListStruct args) throws Exception {
        for (Object arg : args) {
            Expectations.expectList(arg);
            final ListStruct pair = (ListStruct) arg;
            final Object condition = interpreter.eval(pair.car(), context);
            if (asBoolean(condition)) {
                return interpreter.eval(pair.cdar(), context);
            }
        }
        return null;
    }



}

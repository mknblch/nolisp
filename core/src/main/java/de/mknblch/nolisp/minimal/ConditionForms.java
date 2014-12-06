package de.mknblch.nolisp.minimal;

import de.mknblch.nolisp.common.Expectations;
import de.mknblch.nolisp.common.TypeHelper;
import de.mknblch.nolisp.interpreter.Context;
import de.mknblch.nolisp.interpreter.Interpreter;
import de.mknblch.nolisp.datatypes.ListStruct;
import de.mknblch.nolisp.scanner.Constant;
import de.mknblch.nolisp.scanner.Define;
import de.mknblch.nolisp.scanner.Special;

/**
 * @author mknblch
 */
public class ConditionForms {


    @Constant({"true", "TRUE"})
    public static final boolean CONSTANT_TRUE = true;

    @Constant({"false", "FALSE"})
    public static final boolean CONSTANT_FALSE = false;

    @Special
    @Define(value = "if") // (if condition yes no)
    public static Object ifForm(Interpreter interpreter, Context context, ListStruct args) throws Exception {
        final boolean condition = TypeHelper.asBoolean(interpreter.eval(args.car(), context));
        final Object trueBranch = args.cadr();
        final Object falseBranch = args.caddr();
        if (condition) return interpreter.eval(trueBranch, context);
        else return interpreter.eval(falseBranch, context);
    }

    @Special
    @Define(value = "cond")
    public static Object cond(Interpreter interpreter, Context context, ListStruct args) throws Exception {
        for (Object arg : args) {
            Expectations.expectList(arg);
            final ListStruct pair = (ListStruct) arg;
            final Object condition = interpreter.eval(pair.car(), context);
            if (TypeHelper.asBoolean(condition)) {
                return interpreter.eval(pair.cadr(), context);
            }
        }
        return null;
    }


}
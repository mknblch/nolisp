package de.mknblch.sucode.builtin;

import de.mknblch.sucode.ast.ListStruct;
import de.mknblch.sucode.func.Define;
import de.mknblch.sucode.func.Special;
import de.mknblch.sucode.helper.Expectations;
import de.mknblch.sucode.helper.FormatHelper;
import de.mknblch.sucode.helper.TypeHelper;
import de.mknblch.sucode.interpreter.Context;
import de.mknblch.sucode.interpreter.Interpreter;

import static de.mknblch.sucode.helper.TypeHelper.*;

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

    @Special
    @Define(value = "fori") // (fori ( start:INT end:INT [step:INT | 1] ) <form>)
    public static Object foriForm(Interpreter interpreter, Context context, ListStruct args) throws Exception {
        final ListStruct loopArgs = asList(args.car());
        final int from = asInt(interpreter.eval(loopArgs.car(), context)); // from
        final int to = asInt(interpreter.eval(loopArgs.cdar(), context)); // to
        final Object stepRaw = interpreter.eval(loopArgs.cddar(), context); // step if not null
        final int step = isInt(stepRaw) ? asInt(stepRaw) : 1;
        final Object form = args.cdar();
        Object result = null;
        for (int i = from; i < to; i += step) {
            result = interpreter.eval(form, context);
        }
        return result;
    }


}

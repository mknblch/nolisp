package de.mknblch.nolisp.features.basic;

import de.mknblch.nolisp.datatypes.ListStruct;
import de.mknblch.nolisp.datatypes.builtin.BuiltInSpecialForm;
import de.mknblch.nolisp.interpreter.Context;
import de.mknblch.nolisp.interpreter.Interpreter;

import static de.mknblch.nolisp.common.TypeHelper.asInt;
import static de.mknblch.nolisp.common.TypeHelper.asList;
import static de.mknblch.nolisp.common.TypeHelper.isInt;

/**
 * @author mknblch
 */
public class ForISpecialForm extends BuiltInSpecialForm {

    @Override
    public String[] getSymbols() {
        return new String[]{"fori"};
    }

    @Override
    public Object eval(Interpreter interpreter, Context context, ListStruct args) throws Exception {
        final ListStruct loopArgs = asList(args.car());
        final int from = asInt(interpreter.eval(loopArgs.car(), context)); // from
        final int to = asInt(interpreter.eval(loopArgs.cadr(), context)); // to
        final Object stepRaw = interpreter.eval(loopArgs.caddr(), context); // step if not null
        final int step = isInt(stepRaw) ? asInt(stepRaw) : 1;
        final Object form = args.cadr();
        Object result = null;
        for (int i = from; i < to; i += step) {
            result = interpreter.eval(form, context);
        }
        return result;
    }
}
package de.mknblch.nolisp.features.basic;

import de.mknblch.nolisp.common.Expectations;
import de.mknblch.nolisp.common.TypeHelper;
import de.mknblch.nolisp.datatypes.ListStruct;
import de.mknblch.nolisp.datatypes.builtin.BuiltInSpecialForm;
import de.mknblch.nolisp.interpreter.Context;
import de.mknblch.nolisp.interpreter.Interpreter;

/**
 * @author mknblch
 */
public class CondSpecialForm extends BuiltInSpecialForm {

    @Override
    public String[] getSymbols() {
        return new String[]{"cond"};
    }

    @Override
    public Object eval(Interpreter interpreter, Context context, ListStruct args) throws Exception {
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
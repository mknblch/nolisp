package de.mknblch.nolisp.features.lambda;

import de.mknblch.nolisp.common.Expectations;
import de.mknblch.nolisp.datatypes.ListStruct;
import de.mknblch.nolisp.datatypes.builtin.BuiltInSpecialForm;
import de.mknblch.nolisp.interpreter.Context;
import de.mknblch.nolisp.interpreter.Interpreter;

/**
 * @author mknblch
 */
public class FunctionSpecialForm extends BuiltInSpecialForm {

    @Override
    public String[] getSymbols() {
        return new String[]{"function"};
    }

    @Override
    public Object eval(Interpreter interpreter, Context context, ListStruct args) throws Exception {
        final Object car = args.car();
        Expectations.expectQuotedList(car);
        return interpreter.eval(((ListStruct) car).cadr(), context);
    }
}
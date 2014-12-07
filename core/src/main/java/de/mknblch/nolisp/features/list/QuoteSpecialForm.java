package de.mknblch.nolisp.features.list;

import de.mknblch.nolisp.datatypes.ListStruct;
import de.mknblch.nolisp.datatypes.builtin.BuiltInSpecialForm;
import de.mknblch.nolisp.interpreter.Context;
import de.mknblch.nolisp.interpreter.Interpreter;

/**
 * @author mknblch
 */
public class QuoteSpecialForm extends BuiltInSpecialForm {

    @Override
    public String[] getSymbols() {
        return new String[]{"quote"};
    }

    @Override
    public Object eval(Interpreter interpreter, Context context, ListStruct args) throws Exception {
        return args.car();
    }
}
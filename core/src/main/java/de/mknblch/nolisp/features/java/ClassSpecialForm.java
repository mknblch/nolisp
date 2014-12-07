package de.mknblch.nolisp.features.java;

import de.mknblch.nolisp.datatypes.ListStruct;
import de.mknblch.nolisp.datatypes.builtin.BuiltInSpecialForm;
import de.mknblch.nolisp.interpreter.Context;
import de.mknblch.nolisp.interpreter.Interpreter;

import static de.mknblch.nolisp.common.TypeHelper.getSymbolLiteral;

/**
 * @author mknblch
 */
public class ClassSpecialForm extends BuiltInSpecialForm {

    @Override
    public String[] getSymbols() {
        return new String[]{"class"};
    }

    @Override
    public Object eval(Interpreter interpreter, Context context, ListStruct args) throws Exception {
        final Object car = args.car();
        return Class.forName(getSymbolLiteral(car));
    }
}
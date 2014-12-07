package de.mknblch.nolisp.features.java;

import de.mknblch.nolisp.common.Expectations;
import de.mknblch.nolisp.datatypes.ListStruct;
import de.mknblch.nolisp.datatypes.builtin.BuiltInSpecialForm;
import de.mknblch.nolisp.interpreter.Context;
import de.mknblch.nolisp.interpreter.Interpreter;

import static de.mknblch.nolisp.common.TypeHelper.getSymbolLiteral;

/**
 * @author mknblch
 */
public class InstanceOfSpecialForm extends BuiltInSpecialForm {

    @Override
    public String[] getSymbols() {
        return new String[]{"instanceof?"};
    }

    @Override
    public Object eval(Interpreter interpreter, Context context, ListStruct args) throws Exception {
        final String className = getSymbolLiteral(args.car());
        final Object cdar = args.cadr();
        Expectations.expectNotNull(cdar);
        final Object value = interpreter.eval(cdar, context);
        return className.equals(value.getClass().getName());
    }
}
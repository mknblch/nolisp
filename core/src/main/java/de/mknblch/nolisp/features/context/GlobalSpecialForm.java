package de.mknblch.nolisp.features.context;

import de.mknblch.nolisp.common.Expectations;
import de.mknblch.nolisp.datatypes.ListStruct;
import de.mknblch.nolisp.datatypes.builtin.BuiltInSpecialForm;
import de.mknblch.nolisp.interpreter.Context;
import de.mknblch.nolisp.interpreter.Interpreter;

import static de.mknblch.nolisp.common.TypeHelper.getSymbolLiteral;

/**
 * @author mknblch
 */
public class GlobalSpecialForm extends BuiltInSpecialForm {

    @Override
    public String[] getSymbols() {
        return new String[]{"global", "setq"};
    }

    @Override
    public Object eval(Interpreter interpreter, Context context, ListStruct args) throws Exception {
        ListStruct temp = args;
        Object value;
        do {
            Expectations.expectCdr(temp);
            final String key = getSymbolLiteral(temp.car());
            value = interpreter.eval(temp.cdr().car(), context);
            context.bindGlobal(key, value);
            temp = temp.cdr().cdr();
        } while (temp != null);

        return value;
    }
}
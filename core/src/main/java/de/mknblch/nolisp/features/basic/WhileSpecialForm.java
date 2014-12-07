package de.mknblch.nolisp.features.basic;

import de.mknblch.nolisp.datatypes.ListStruct;
import de.mknblch.nolisp.datatypes.forms.BuiltInSpecialForm;
import de.mknblch.nolisp.interpreter.Context;
import de.mknblch.nolisp.interpreter.Interpreter;

import static de.mknblch.nolisp.common.TypeHelper.asBoolean;
import static de.mknblch.nolisp.common.TypeHelper.asList;

/**
 * @author mknblch
 */
public class WhileSpecialForm extends BuiltInSpecialForm {

    @Override
    public String[] getSymbols() {
        return new String[]{"while"};
    }

    @Override
    public Object eval(Interpreter interpreter, Context context, ListStruct args) throws Exception {
        final Object condition = args.car();
        final ListStruct forms = asList(args.cadr());
        ListStruct r = null;
        while (asBoolean(interpreter.eval(condition, context))) {
            r = interpreter.evalEach(forms, context);
        }
        return null != r ? r.last().car() : null;
    }
}
package de.mknblch.nolisp.features.lambda;

import de.mknblch.nolisp.common.TypeHelper;
import de.mknblch.nolisp.datatypes.ListStruct;
import de.mknblch.nolisp.datatypes.forms.BuiltInSpecialForm;
import de.mknblch.nolisp.datatypes.forms.Form;
import de.mknblch.nolisp.interpreter.Context;
import de.mknblch.nolisp.interpreter.Interpreter;

/**
 * @author mknblch
 */
public class FunCallSpecialForm extends BuiltInSpecialForm {

    @Override
    public String[] getSymbols() {
        return new String[]{"funcall"};
    }

    @Override
    public Object eval(Interpreter interpreter, Context context, ListStruct args) throws Exception {
        final Object car = args.car();
        final Form form = TypeHelper.asForm(interpreter.eval(car, context));
        Object ret = interpreter.eval(args.cadr(), context);
        ListStruct rest = args.cddr();
        for (Object o : rest) {
            ret = form.eval(new ListStruct(ret, interpreter.eval(o, context)));
        }
        return ret;
    }
}
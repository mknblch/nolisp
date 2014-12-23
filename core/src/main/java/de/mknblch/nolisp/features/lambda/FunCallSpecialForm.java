package de.mknblch.nolisp.features.lambda;

import de.mknblch.nolisp.common.TypeHelper;
import de.mknblch.nolisp.datatypes.ListStruct;
import de.mknblch.nolisp.datatypes.builtin.BuiltInSpecialForm;
import de.mknblch.nolisp.datatypes.Form;
import de.mknblch.nolisp.interpreter.Context;
import de.mknblch.nolisp.interpreter.Interpreter;
import de.mknblch.nolisp.codegen.Define;

/**
 * @author mknblch
 */
@Define({"funcall"})
public class FunCallSpecialForm extends BuiltInSpecialForm  {

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
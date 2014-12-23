package de.mknblch.nolisp.features.loops;

import de.mknblch.nolisp.common.TypeHelper;
import de.mknblch.nolisp.datatypes.ListStruct;
import de.mknblch.nolisp.datatypes.builtin.BuiltInSpecialForm;
import de.mknblch.nolisp.interpreter.Context;
import de.mknblch.nolisp.interpreter.Interpreter;
import de.mknblch.nolisp.generator.Define;

import static de.mknblch.nolisp.common.TypeHelper.asBoolean;
import static de.mknblch.nolisp.common.TypeHelper.asList;

/**
 * @author mknblch
 */
@Define({"while"})
public class WhileSpecialForm extends BuiltInSpecialForm  {

    @Override
    public Object eval(Interpreter interpreter, Context context, ListStruct args) throws Exception {
        final Object condition = args.car();
        final ListStruct forms = TypeHelper.asList(args.cadr());
        ListStruct r = null;
        while (TypeHelper.asBoolean(interpreter.eval(condition, context))) {
            r = interpreter.evalEach(forms, context);
        }
        return null != r ? r.last().car() : null;
    }
}
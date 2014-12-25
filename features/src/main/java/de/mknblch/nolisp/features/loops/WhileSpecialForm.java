package de.mknblch.nolisp.features.loops;

import de.mknblch.nolisp.common.TypeHelper;
import de.mknblch.nolisp.datatypes.ListStruct;
import de.mknblch.nolisp.datatypes.SpecialForm;
import de.mknblch.nolisp.generator.Define;
import de.mknblch.nolisp.interpreter.Context;
import de.mknblch.nolisp.interpreter.Interpreter;

/**
 * @author mknblch
 */
@Define({"while"})
public class WhileSpecialForm implements SpecialForm {

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
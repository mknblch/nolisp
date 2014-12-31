package de.mknblch.nolisp.features.minimal.loops;

import de.mknblch.nolisp.common.TypeHelper;
import de.mknblch.nolisp.datatypes.ListStruct;
import de.mknblch.nolisp.datatypes.SpecialForm;
import de.mknblch.nolisp.generator.annotations.Define;
import de.mknblch.nolisp.interpreter.Context;
import de.mknblch.nolisp.interpreter.Interpreter;

/**
 * @author mknblch
 */
@Define({"fori"})
public class ForISpecialForm implements SpecialForm {

    @Override
    public Object eval(Interpreter interpreter, Context context, ListStruct args) throws Exception {
        final ListStruct loopArgs = TypeHelper.asList(args.car());
        final int from = TypeHelper.asInt(interpreter.eval(loopArgs.car(), context)); // from
        final int to = TypeHelper.asInt(interpreter.eval(loopArgs.cadr(), context)); // to
        final Object stepRaw = interpreter.eval(loopArgs.caddr(), context); // step if not null
        final int step = TypeHelper.isInt(stepRaw) ? TypeHelper.asInt(stepRaw) : 1;
        final Object form = args.cadr();
        Object result = null;
        for (int i = from; i < to; i += step) {
            result = interpreter.eval(form, context);
        }
        return result;
    }
}
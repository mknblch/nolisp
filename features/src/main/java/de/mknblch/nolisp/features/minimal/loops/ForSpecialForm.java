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
@Define({"for"})
public class ForSpecialForm implements SpecialForm {

    @Override
    public Object eval(Interpreter interpreter, Context context, ListStruct args) throws Exception {
        final Context localScope = context.derive();
        final ListStruct loopArgs = TypeHelper.asList(args.car());
        final String literal = TypeHelper.getSymbolLiteral(loopArgs.car()); // sym
        final int from = TypeHelper.asInt(interpreter.eval(loopArgs.cadr(), context)); // from
        final int to = TypeHelper.asInt(interpreter.eval(loopArgs.caddr(), context)); // to
        final Object stepRaw = interpreter.eval(loopArgs.cadddr(), context); // step if not null
        final int step = TypeHelper.isInt(stepRaw) ? TypeHelper.asInt(stepRaw) : 1;
        final ListStruct forms = TypeHelper.asList(args.cadr());
        Object result = null;
        for (int i = from; i < to; i += step) {
            localScope.bind(literal, i);
            for (Object form : forms) {
                result = interpreter.eval(form, localScope);
            }
        }
        return result;
    }
}
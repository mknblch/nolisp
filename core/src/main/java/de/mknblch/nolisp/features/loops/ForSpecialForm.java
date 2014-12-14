package de.mknblch.nolisp.features.loops;

import de.mknblch.nolisp.datatypes.ListStruct;
import de.mknblch.nolisp.datatypes.builtin.BuiltInSpecialForm;
import de.mknblch.nolisp.interpreter.Context;
import de.mknblch.nolisp.interpreter.Interpreter;
import de.mknblch.nolisp.dialect.Define;

import static de.mknblch.nolisp.common.TypeHelper.*;
import static de.mknblch.nolisp.common.TypeHelper.asInt;
import static de.mknblch.nolisp.common.TypeHelper.asList;

/**
 * @author mknblch
 */
@Define({"for"})
public class ForSpecialForm extends BuiltInSpecialForm  {

    @Override
    public Object eval(Interpreter interpreter, Context context, ListStruct args) throws Exception {
        final Context localScope = context.derive();
        final ListStruct loopArgs = asList(args.car());
        final String literal = getSymbolLiteral(loopArgs.car()); // sym
        final int from = asInt(interpreter.eval(loopArgs.cadr(), context)); // from
        final int to = asInt(interpreter.eval(loopArgs.caddr(), context)); // to
        final Object stepRaw = interpreter.eval(loopArgs.cadddr(), context); // step if not null
        final int step = isInt(stepRaw) ? asInt(stepRaw) : 1;
        final ListStruct forms = asList(args.cadr());
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
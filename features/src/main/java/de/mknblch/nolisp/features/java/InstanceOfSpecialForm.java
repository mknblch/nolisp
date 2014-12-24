package de.mknblch.nolisp.features.java;

import de.mknblch.nolisp.common.Expectations;
import de.mknblch.nolisp.common.TypeHelper;
import de.mknblch.nolisp.datatypes.ListStruct;
import de.mknblch.nolisp.datatypes.builtin.BuiltInSpecialForm;
import de.mknblch.nolisp.interpreter.Context;
import de.mknblch.nolisp.interpreter.Interpreter;
import de.mknblch.nolisp.generator.Define;

import static de.mknblch.nolisp.common.TypeHelper.getSymbolLiteral;

/**
 * @author mknblch
 */
@Define({"instanceof?"})
public class InstanceOfSpecialForm extends BuiltInSpecialForm  {

    @Override
    public Object eval(Interpreter interpreter, Context context, ListStruct args) throws Exception {
        final String className = TypeHelper.getSymbolLiteral(args.car());
        final Object cdar = args.cadr();
        Expectations.expectNotNull(cdar);
        final Object value = interpreter.eval(cdar, context);
        return className.equals(value.getClass().getName());
    }
}
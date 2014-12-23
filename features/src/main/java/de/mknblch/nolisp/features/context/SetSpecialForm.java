package de.mknblch.nolisp.features.context;

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
@Define({"set"})
public class SetSpecialForm extends BuiltInSpecialForm  {

    @Override
    public Object eval(Interpreter interpreter, Context context, ListStruct args) throws Exception {
        ListStruct temp = args;
        Object value;
        do {
            final String key = TypeHelper.getSymbolLiteral(temp.car());
            Expectations.expectCdr(temp);
            value = interpreter.eval(temp.cdr().car(), context);
            context.bindToContainer(key, value);
            temp = temp.cdr().cdr();
        } while (temp != null);

        return value;
    }
}
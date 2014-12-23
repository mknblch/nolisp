package de.mknblch.nolisp.features.list;

import de.mknblch.nolisp.datatypes.ListStruct;
import de.mknblch.nolisp.datatypes.builtin.BuiltInSpecialForm;
import de.mknblch.nolisp.interpreter.Context;
import de.mknblch.nolisp.interpreter.Interpreter;
import de.mknblch.nolisp.generator.Define;

/**
 * @author mknblch
 */
@Define({"quote"})
public class QuoteSpecialForm extends BuiltInSpecialForm  {

    @Override
    public Object eval(Interpreter interpreter, Context context, ListStruct args) throws Exception {
        return args.car();
    }
}
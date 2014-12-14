package de.mknblch.nolisp.features.lambda;

import de.mknblch.nolisp.datatypes.ListStruct;
import de.mknblch.nolisp.datatypes.builtin.BuiltInSpecialForm;
import de.mknblch.nolisp.interpreter.Context;
import de.mknblch.nolisp.interpreter.Interpreter;
import de.mknblch.nolisp.scanner.Define;

/**
 * @author mknblch
 */
@Define({"eval"})
public class EvalSpecialForm extends BuiltInSpecialForm  {

    @Override
    public Object eval(Interpreter interpreter, Context context, ListStruct args) throws Exception {
        return interpreter.eval(interpreter.eval(args.car(), context), context);
    }
}
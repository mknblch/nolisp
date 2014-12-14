package de.mknblch.nolisp.features.minimal;

import de.mknblch.nolisp.common.TypeHelper;
import de.mknblch.nolisp.datatypes.ListStruct;
import de.mknblch.nolisp.datatypes.builtin.BuiltInSpecialForm;
import de.mknblch.nolisp.interpreter.Context;
import de.mknblch.nolisp.interpreter.Interpreter;
import de.mknblch.nolisp.scanner.Define;

/**
 * @author mknblch
 */
@Define({"if"})
public class IfSpecialForm extends BuiltInSpecialForm {

    @Override
    public Object eval(Interpreter interpreter, Context context, ListStruct args) throws Exception {
        final boolean condition = TypeHelper.asBoolean(interpreter.eval(args.car(), context));
        final Object trueBranch = args.cadr();
        final Object falseBranch = args.caddr();
        if (condition) return interpreter.eval(trueBranch, context);
        else return interpreter.eval(falseBranch, context);
    }
}
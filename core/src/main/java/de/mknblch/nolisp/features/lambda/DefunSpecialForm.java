package de.mknblch.nolisp.features.lambda;

import de.mknblch.nolisp.common.Expectations;
import de.mknblch.nolisp.common.TypeHelper;
import de.mknblch.nolisp.datatypes.ListStruct;
import de.mknblch.nolisp.datatypes.forms.BuiltInSpecialForm;
import de.mknblch.nolisp.datatypes.forms.Lambda;
import de.mknblch.nolisp.interpreter.Context;
import de.mknblch.nolisp.interpreter.Interpreter;

/**
 * @author mknblch
 */
public class DefunSpecialForm extends BuiltInSpecialForm {

    @Override
    public String[] getSymbols() {
        return new String[]{"defun"};
    }

    @Override
    public Object eval(Interpreter interpreter, Context context, ListStruct args) throws Exception {
        Expectations.expectCdr(args);
        final String functionName = TypeHelper.getSymbolLiteral(args.car());
        final Lambda lambda = new Lambda(interpreter, context, TypeHelper.asList(args.cadr()), args.caddr());
        context.bindGlobal(functionName, lambda);
        return lambda;
    }
}
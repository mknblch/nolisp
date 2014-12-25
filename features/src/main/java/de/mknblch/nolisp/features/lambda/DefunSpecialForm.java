package de.mknblch.nolisp.features.lambda;

import de.mknblch.nolisp.common.Expectations;
import de.mknblch.nolisp.common.TypeHelper;
import de.mknblch.nolisp.datatypes.ListStruct;
import de.mknblch.nolisp.datatypes.SpecialForm;
import de.mknblch.nolisp.generator.Define;
import de.mknblch.nolisp.interpreter.Context;
import de.mknblch.nolisp.interpreter.Interpreter;

/**
 * @author mknblch
 */
@Define({"defun"})
public class DefunSpecialForm implements SpecialForm {

    @Override
    public Object eval(Interpreter interpreter, Context context, ListStruct args) throws Exception {
        Expectations.expectCdr(args);
        final String functionName = TypeHelper.getSymbolLiteral(args.car());
        final Lambda lambda = new Lambda(interpreter, context, TypeHelper.asList(args.cadr()), args.caddr());
        context.bindGlobal(functionName, lambda);
        return lambda;
    }
}
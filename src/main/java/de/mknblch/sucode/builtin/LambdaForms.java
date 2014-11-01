package de.mknblch.sucode.builtin;

import de.mknblch.sucode.ast.ListStruct;
import de.mknblch.sucode.ast.forms.Form;
import de.mknblch.sucode.ast.forms.LambdaForm;
import de.mknblch.sucode.func.Define;
import de.mknblch.sucode.func.Special;
import de.mknblch.sucode.interpreter.Context;
import de.mknblch.sucode.interpreter.Interpreter;

import java.util.List;

import static de.mknblch.sucode.helper.Expectations.expectCdr;
import static de.mknblch.sucode.helper.TypeHelper.asForm;
import static de.mknblch.sucode.helper.TypeHelper.symbolList;
import static de.mknblch.sucode.helper.TypeHelper.symbolLiteral;

/**
 * @author mknblch
 */
public class LambdaForms {

    @Special
    @Define("lambda") // ((lambda (a) (+ a 1)) 1) => 2
    public static Object lambda(Interpreter interpreter, Context parentContext, ListStruct args) throws Exception {
        expectCdr(args);
        return new LambdaForm(interpreter, parentContext, symbolList(args.car()), args.cdar());
    }

    @Special
    @Define("defun") // (defun bla (a) (+ a 1) ) => form
    public static Object defun(Interpreter interpreter, Context parentContext, ListStruct args) throws Exception {
        // TODO review
        expectCdr(args);
        final String functionName = symbolLiteral(args.car());
        final List<String> symbols = symbolList(args.cdar());
        final LambdaForm lambda = new LambdaForm(interpreter, parentContext, symbols, args.cddar());
        parentContext.bindGlobal(functionName, lambda);
        return lambda;
    }

    @Special
    @Define("function") // (function +)
    public static Object function(Interpreter interpreter, Context parentContext, ListStruct args) throws Exception {
//       TODO REVIEW & TEST
        final Object eval = interpreter.eval(args.car(), parentContext);
        return interpreter.eval(eval, parentContext);
    }

    @Special
    @Define("funcall") // (funcall (function +) 1 2 3 4 5) => 15
    public static Object funcall(Interpreter interpreter, Context parentContext, ListStruct args) throws Exception {
        final Object car = args.car();
        final Form form = asForm(interpreter.eval(car, parentContext));
        Object ret = args.cdar();
        ListStruct rest = args.cddr();
        for (Object o : rest) {
            ret = form.eval(parentContext, new ListStruct(ret, interpreter.eval(o, parentContext)));
        }
        return ret;
    }


}

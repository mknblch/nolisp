package de.mknblch.nolisp.minimallisp;

import de.mknblch.nolisp.ast.ListStruct;
import de.mknblch.nolisp.ast.forms.Form;
import de.mknblch.nolisp.ast.forms.LambdaForm;
import de.mknblch.nolisp.annotations.Define;
import de.mknblch.nolisp.annotations.Special;
import de.mknblch.nolisp.interpreter.Context;
import de.mknblch.nolisp.interpreter.Interpreter;

import java.util.List;

import static de.mknblch.nolisp.helper.Expectations.expectCdr;
import static de.mknblch.nolisp.helper.Expectations.expectQuotedList;
import static de.mknblch.nolisp.helper.TypeHelper.asForm;
import static de.mknblch.nolisp.helper.TypeHelper.symbolList;
import static de.mknblch.nolisp.helper.TypeHelper.symbolLiteral;

/**
 * @author mknblch
 */
public class LambdaForms {

    @Special
    @Define("lambda") // ((lambda (a) (+ a 1)) 1) => 2
    public static Object lambda(Interpreter interpreter, Context parentContext, ListStruct args) throws Exception {
        expectCdr(args);
        return new LambdaForm(interpreter, parentContext.derive(), symbolList(args.car()), args.cdar());
    }

    @Special
    @Define("defun") // (defun bla (a) (+ a 1) ) => form
    public static Object defun(Interpreter interpreter, Context parentContext, ListStruct args) throws Exception {
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
        final Object car = args.car();
        expectQuotedList(car);
        return interpreter.eval(((ListStruct) car).cdar(), parentContext);
    }

    @Special
    @Define("funcall") // (funcall (function +) 1 2 3 4 5) => 15
    public static Object funcall(Interpreter interpreter, Context parentContext, ListStruct args) throws Exception {
        final Object car = args.car();
        final Form form = asForm(interpreter.eval(car, parentContext));
        Object ret = interpreter.eval(args.cdar(), parentContext);
        ListStruct rest = args.cddr();
        for (Object o : rest) {
            ret = form.eval(parentContext, new ListStruct(ret, interpreter.eval(o, parentContext)));
        }
        return ret;
    }

    @Special
    @Define("eval") // (eval '(+ 20 22)) => (eval (quote (+ 20 22))) => 42
    public static Object eval(Interpreter interpreter, Context parentContext, ListStruct args) throws Exception {
        return interpreter.eval(interpreter.eval(args.car(), parentContext), parentContext);
    }


}

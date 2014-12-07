package de.mknblch.nolisp.minimal;

import de.mknblch.nolisp.common.Expectations;
import de.mknblch.nolisp.common.TypeHelper;
import de.mknblch.nolisp.interpreter.Context;
import de.mknblch.nolisp.interpreter.Interpreter;
import de.mknblch.nolisp.datatypes.ListStruct;
import de.mknblch.nolisp.datatypes.forms.Form;
import de.mknblch.nolisp.datatypes.forms.Lambda;
import de.mknblch.nolisp.scanner.Define;
import de.mknblch.nolisp.scanner.Special;

/**
 * @author mknblch
 */
public class LambdaForms {

    @Special
    @Define("lambda") // ((lambda (a) (+ a 1)) 1) => 2
    public static Object lambda(Interpreter interpreter, Context context, ListStruct args) throws Exception {
        Expectations.expectCdr(args);
        return new Lambda(interpreter, context, TypeHelper.asList(args.car()), args.cadr());
    }

    @Define("lbody") // (lbody (lambda (a) (+ a 1))) => (+ a 1)
    public static Object lbody(ListStruct args) throws Exception {
        return TypeHelper.asLambda(args.car()).getForm();
    }

    @Define("largs") // (lbody (lambda (a) (+ a 1))) => (+ a 1)
    public static Object largs(ListStruct args) throws Exception {
        return TypeHelper.asLambda(args.car()).getArgumentSymbols();
    }

    @Special
    @Define("defun") // (defun bla (a) (+ a 1) ) => form
    public static Object defun(Interpreter interpreter, Context context, ListStruct args) throws Exception {
        Expectations.expectCdr(args);
        final String functionName = TypeHelper.getSymbolLiteral(args.car());
        final Lambda lambda = new Lambda(interpreter, context, TypeHelper.asList(args.cadr()), args.caddr());
        context.bindGlobal(functionName, lambda);
        return lambda;
    }

    @Special
    @Define("function") // (function +)
    public static Object function(Interpreter interpreter, Context parentContext, ListStruct args) throws Exception {
        final Object car = args.car();
        Expectations.expectQuotedList(car);
        return interpreter.eval(((ListStruct) car).cadr(), parentContext);
    }

    @Special
    @Define("funcall") // (funcall (function +) 1 2 3 4 5) => 15
    public static Object funcall(Interpreter interpreter, Context parentContext, ListStruct args) throws Exception {
        final Object car = args.car();
        final Form form = TypeHelper.asForm(interpreter.eval(car, parentContext));
        Object ret = interpreter.eval(args.cadr(), parentContext);
        ListStruct rest = args.cddr();
        for (Object o : rest) {
            ret = form.eval(new ListStruct(ret, interpreter.eval(o, parentContext)));
        }
        return ret;
    }

    @Special
    @Define("eval") // (eval '(+ 20 22)) => (eval (quote (+ 20 22))) => 42
    public static Object eval(Interpreter interpreter, Context parentContext, ListStruct args) throws Exception {
        return interpreter.eval(interpreter.eval(args.car(), parentContext), parentContext);
    }


}

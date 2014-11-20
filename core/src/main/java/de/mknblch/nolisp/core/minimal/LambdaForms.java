package de.mknblch.nolisp.core.minimal;

import de.mknblch.nolisp.core.common.Expectations;
import de.mknblch.nolisp.core.common.TypeHelper;
import de.mknblch.nolisp.core.interpreter.Context;
import de.mknblch.nolisp.core.interpreter.Interpreter;
import de.mknblch.nolisp.core.interpreter.structs.ListStruct;
import de.mknblch.nolisp.core.interpreter.structs.forms.Form;
import de.mknblch.nolisp.core.interpreter.structs.forms.LambdaForm;
import de.mknblch.nolisp.core.scanner.Define;
import de.mknblch.nolisp.core.scanner.Special;

import java.util.List;

/**
 * @author mknblch
 */
public class LambdaForms {

    @Special
    @Define("lambda") // ((lambda (a) (+ a 1)) 1) => 2
    public static Object lambda(Interpreter interpreter, Context context, ListStruct args) throws Exception {
        Expectations.expectCdr(args);
        return new LambdaForm(interpreter, context, TypeHelper.symbolList(args.car()), args.cdar());
    }

    @Define("lbody") // (lbody (lambda (a) (+ a 1))) => (+ a 1)
    public static Object lbody(ListStruct args) throws Exception {
        final LambdaForm lambda = TypeHelper.asLambda(args.car());
        return lambda.getForm();
    }

    @Define("largs") // (lbody (lambda (a) (+ a 1))) => (+ a 1)
    public static Object largs(ListStruct args) throws Exception {
        final LambdaForm lambda = TypeHelper.asLambda(args.car());
        return TypeHelper.javaListToListStruct(lambda.getArgumentSymbols());
    }

    @Special
    @Define("defun") // (defun bla (a) (+ a 1) ) => form
    public static Object defun(Interpreter interpreter, Context context, ListStruct args) throws Exception {
        Expectations.expectCdr(args);
        final String functionName = TypeHelper.symbolLiteral(args.car());
        final List<String> symbols = TypeHelper.symbolList(args.cdar());
        final LambdaForm lambda = new LambdaForm(interpreter, context, symbols, args.cddar());
        context.bindGlobal(functionName, lambda);
        return lambda;
    }

    @Special
    @Define("function") // (function +)
    public static Object function(Interpreter interpreter, Context parentContext, ListStruct args) throws Exception {
        final Object car = args.car();
        Expectations.expectQuotedList(car);
        return interpreter.eval(((ListStruct) car).cdar(), parentContext);
    }

    @Special
    @Define("funcall") // (funcall (function +) 1 2 3 4 5) => 15
    public static Object funcall(Interpreter interpreter, Context parentContext, ListStruct args) throws Exception {
        final Object car = args.car();
        final Form form = TypeHelper.asForm(interpreter.eval(car, parentContext));
        Object ret = interpreter.eval(args.cdar(), parentContext);
        ListStruct rest = args.cddr();
        for (Object o : rest) {
            ret = form.eval(new ListStruct(ret, interpreter.eval(o, parentContext)));
        }
        return ret;
    }

    @Special
    @Define("eval") // (eval '(+ 20 22)) => (eval (quote (+ 20 22))) => 42
    public static Object eval(Interpreter interpreter, Context parentContext, ListStruct args) throws Exception {
        return interpreter.eval(interpreter.eval(args.car(), parentContext), parentContext); // TODO review: replace double eval with asList ?
    }


}

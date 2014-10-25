package de.mknblch.sucode.builtin;

import de.mknblch.sucode.ast.forms.Form;
import de.mknblch.sucode.ast.forms.LambdaForm;
import de.mknblch.sucode.func.*;
import de.mknblch.sucode.helper.FormatHelper;
import de.mknblch.sucode.interpreter.Context;
import de.mknblch.sucode.interpreter.EvaluationException;
import de.mknblch.sucode.interpreter.Interpreter;
import de.mknblch.sucode.ast.ListStruct;

import java.util.List;

import static de.mknblch.sucode.helper.TypeHelper.*;

/**
 * @author mknblch
 */
public class SpecialForms {

    @Special
    @Define(value = "setq")
    public static Object setq(Interpreter interpreter, Context context, ListStruct args) throws Exception {
        ListStruct temp = args;
        Object value;
        do {
            final String key = symbolLiteral(temp.car());
            expectCdr(temp);
            value = interpreter.eval(temp.cdr().car(), context);
            context.bindGlobal(key, value);
            temp = temp.cdr().cdr();
        } while (temp != null);

        return value;
    }

    @Special
    @Define(value = "quote")
    public static Object quote(Interpreter interpreter, Context context, ListStruct args) throws EvaluationException {
        return args.car();
    }

    @Define(value = "list")
    public static Object list(Context context, ListStruct args) throws EvaluationException {
        return args;
    }

    @Special
    @Define(value = "let*") // (let* ((a 1) (b a)) b) => 1
    public static Object letAsterisk(Interpreter interpreter, Context parentScope, ListStruct args) throws Exception {
        final Context localScope = parentScope.derive();
        // car must be list
        for (Object def : (ListStruct) args.car()) {
            // each element must be a value-value pair.
            final ListStruct pair = ((ListStruct) def);
            expectCdr(pair);
            // bind to local and eval with local scope
            localScope.bind(symbolLiteral(pair.car()), interpreter.eval(pair.cdr().car(), localScope));
        }
        // evaluate cdar with newly build variable scope
        return interpreter.eval(args.cdr().car(), localScope);
    }

    @Special
    @Define(value = "let") // (let ((a 1) (b a)) b) => ERROR
    public static Object let(Interpreter interpreter, Context parentScope, ListStruct args) throws Exception {
        final Context localScope = parentScope.derive();
        // car must be list
        final ListStruct car = (ListStruct) args.car();
        expectList(car);
        for (Object def : car) {
            // each element must be a value-value pair.
            final ListStruct pair = ((ListStruct) def);
            expectCdr(pair);
            // bind to local but eval args with parent scope
            localScope.bind(symbolLiteral(pair.car()), interpreter.eval(pair.cdr().car(), parentScope));
        }
        return interpreter.eval(args.cdr().car(), localScope);
    }

    @Define(value = "progn") // (progn 1 2 3) => 3
    public static Object progn(Context parentScope, ListStruct args) throws Exception {
        if (null == args) {
            return null;
        }
        return args.last().car();
    }

    @Special
    @Define(value = "lambda") // ((lambda (a) (+ a 1)) 1) => 2
    public static Object lambda(Interpreter interpreter, Context parentContext, ListStruct args) throws Exception {
        expectCdr(args);
        return new LambdaForm(interpreter, parentContext, symbolList(args.car()), args.cdar());
    }

    @Special
    @Define(value = "defmacro") // ((lambda (a) (+ a 1)) 1) => 2
    public static Object defmacro(Interpreter interpreter, Context parentContext, ListStruct args) throws Exception {
        expectCdr(args);
        throw new EvaluationException("defmacro not yet implemented");

//        return new LambdaForm(interpreter, parentContext, symbolList(args.car()), args.cdar());
    }

    @Special
    @Define(value = "defun") // (defun bla (a) (+ a 1) ) => form
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
    @Define(value = "eval") // (eval '(+ 20 22)) :> (eval (quote (+ 20 22))) => 42
    public static Object eval(Interpreter interpreter, Context parentContext, ListStruct args) throws Exception {
        return interpreter.eval(interpreter.eval(args.car(), parentContext), parentContext);
    }

    @Special
    @Define(value = "function") // (function +)
    public static Object function(Interpreter interpreter, Context parentContext, ListStruct args) throws Exception {
        return args.car();
    }

    @Special
    @Define(value = "funcall") // (funcall (function +) 1 2 3 4 5) => 15
    public static Object funcall(Interpreter interpreter, Context parentContext, ListStruct args) throws Exception {
        final String symbol = symbolLiteral(interpreter.eval(args.car(), parentContext));
        final Form form = asForm(parentContext.get(symbol));
        Object ret = args.cdar();
        ListStruct rest = args.cddr();
        for (Object o : rest) {
            ret = form.eval(parentContext, new ListStruct(ret, interpreter.eval(o, parentContext)));
        }
        return ret;
    }

}

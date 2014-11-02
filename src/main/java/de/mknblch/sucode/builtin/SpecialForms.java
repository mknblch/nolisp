package de.mknblch.sucode.builtin;

import de.mknblch.sucode.ast.ListStruct;
import de.mknblch.sucode.func.Define;
import de.mknblch.sucode.func.Special;
import de.mknblch.sucode.interpreter.Context;
import de.mknblch.sucode.interpreter.EvaluationException;
import de.mknblch.sucode.interpreter.Interpreter;

import static de.mknblch.sucode.helper.Expectations.expectCdr;
import static de.mknblch.sucode.helper.Expectations.expectList;
import static de.mknblch.sucode.helper.TypeHelper.*;

/**
 * @author mknblch
 */
public class SpecialForms {

    @Special
    @Define("setq")
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
    @Define("quote")
    public static Object quote(Interpreter interpreter, Context context, ListStruct args) throws EvaluationException {
        return args.car();
    }

    @Define("list")
    public static Object list(Context context, ListStruct args) throws EvaluationException {
        return args;
    }

    @Special
    @Define("let*") // (let* ((a 1) (b a)) b) => 1
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
    @Define("let") // (let ((a 1) (b a)) b) => ERROR
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

    @Define("progn") // (progn 1 2 3) => 3
    public static Object progn(Context parentScope, ListStruct args) throws Exception {
        if (null == args) {
            return null;
        }
        return args.last().car();
    }

    @Special
    @Define("eval") // (eval '(+ 20 22)) :> (eval (quote (+ 20 22))) => 42
    public static Object eval(Interpreter interpreter, Context parentContext, ListStruct args) throws Exception {
        // TODO Test
        return interpreter.eval(interpreter.eval(args.car(), parentContext), parentContext);
    }

    @Special
    @Define(value = "fori") // (fori ( start:INT end:INT [step:INT | 1] ) <form>)
    public static Object fori(Interpreter interpreter, Context context, ListStruct args) throws Exception {
        final ListStruct loopArgs = asList(args.car());
        final int from = asInt(interpreter.eval(loopArgs.car(), context)); // from
        final int to = asInt(interpreter.eval(loopArgs.cdar(), context)); // to
        final Object stepRaw = interpreter.eval(loopArgs.cddar(), context); // step if not null
        final int step = isInt(stepRaw) ? asInt(stepRaw) : 1;
        final Object form = args.cdar();
        Object result = null;
        for (int i = from; i < to; i += step) {
            result = interpreter.eval(form, context);
        }
        return result;
    }
}

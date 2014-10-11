package de.mknblch.sucode.interpreter.func.builtin;

import de.mknblch.sucode.interpreter.Context;
import de.mknblch.sucode.interpreter.EvaluationException;
import de.mknblch.sucode.interpreter.Interpreter;
import de.mknblch.sucode.interpreter.func.Define;
import de.mknblch.sucode.interpreter.func.TypeHelper;
import de.mknblch.sucode.parser.structs.ListStruct;

/**
 * Created by mknblch on 12.10.2014.
 */
public class SpecialForms {

    @Define(symbol = "quote", special = true)
    public static Object quote(ListStruct args, Context context) throws EvaluationException {
        return args.car();
    }

    @Define(symbol = "let", special = true) // (let ((a 1) (b a)) b) => ERROR
    public static Object let(ListStruct args, Context env) throws Exception {
        final Context localScope = env.derive();
        // car must be list
        for (Object def : (ListStruct) args.car()) {
            // each element must be a symbol-value pair.
            final ListStruct pair = ((ListStruct) def);
            // bind to local but eval args with parent env
            localScope.bind(TypeHelper.symbolLiteral(pair.car()), Interpreter.eval(pair.cdr().car(), env));
        }
        return Interpreter.eval(args.cdr().car(), localScope);
    }

    @Define(symbol = "let*", special = true) // (let* ((a 1) (b a)) b) => 1
    public static Object letAsterisk(ListStruct args, Context env) throws Exception {

        final Context localScope = env.derive();
        // car must be list
        for (Object def : (ListStruct) args.car()) {
            // each element must be a symbol-value pair.
            final ListStruct pair = ((ListStruct) def);
            // bind to local and eval with local scope
            localScope.bind(TypeHelper.symbolLiteral(pair.car()), Interpreter.eval(pair.cdr().car(), localScope));
        }
        // evaluate cdar with newly build variable scope
        return Interpreter.eval(args.cdr().car(), localScope);
    }


    @Define(symbol = "lambda", special = true) // ((lambda (a) (+ a 1)) 1) => 2
    public static Object lambda(ListStruct args, Context env) throws Exception {

        return null;
    }

}

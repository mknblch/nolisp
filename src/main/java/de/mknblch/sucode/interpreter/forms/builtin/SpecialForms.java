package de.mknblch.sucode.interpreter.forms.builtin;

import de.mknblch.sucode.interpreter.Environment;
import de.mknblch.sucode.interpreter.EvaluationException;
import de.mknblch.sucode.interpreter.Interpreter;
import de.mknblch.sucode.interpreter.forms.Function;
import de.mknblch.sucode.interpreter.forms.TypeHelper;
import de.mknblch.sucode.parser.structs.ListStruct;
import de.mknblch.sucode.parser.structs.SymbolStruct;

import java.util.List;

/**
 * Created by mknblch on 12.10.2014.
 */
public class SpecialForms {

    @Function(symbol = "quote")
    public static Object quote(ListStruct args, Environment env, Interpreter ip) throws EvaluationException {
        return args.car();
    }

    @Function(symbol = "let") // (let ((a 1) (b a)) b) => null
    public static Object let(ListStruct args, Environment env, Interpreter ip) throws Exception {
        final Environment localScope = env.derive();
        // car must be list
        for (Object def : (ListStruct) args.car()) {
            // each element must be a symbol-value pair.
            final ListStruct pair = ((ListStruct) def);
            // bind to local but eval args with parent env
            localScope.bind(TypeHelper.symbolLiteral(pair.car()), ip.eval(pair.cdr().car(), env));
        }
        return ip.eval(args.cdr().car(), localScope);
    }

    @Function(symbol = "let*") // (let* ((a 1) (b a)) b) => 1
    public static Object letAsterisk(ListStruct args, Environment env, Interpreter ip) throws Exception {

        final Environment localScope = env.derive();
        // car must be list
        for (Object def : (ListStruct) args.car()) {
            // each element must be a symbol-value pair.
            final ListStruct pair = ((ListStruct) def);
            // bind to local and eval with local scope
            localScope.bind(TypeHelper.symbolLiteral(pair.car()), ip.eval(pair.cdr().car(), localScope));
        }
        // evaluate cdar with newly build variable scope
        return ip.eval(args.cdr().car(), localScope);
    }


    @Function(symbol = "lambda") // ((lambda (a) (+ a 1)) 1) => 2
    public static Object lambda(ListStruct args, Environment env, Interpreter ip) throws Exception {
        return null;
    }

}

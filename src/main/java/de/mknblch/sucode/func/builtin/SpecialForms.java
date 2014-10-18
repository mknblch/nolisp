package de.mknblch.sucode.func.builtin;

import de.mknblch.sucode.func.*;
import de.mknblch.sucode.interpreter.Context;
import de.mknblch.sucode.interpreter.EvaluationException;
import de.mknblch.sucode.interpreter.Interpreter;
import de.mknblch.sucode.structs.ListStruct;

import java.util.List;

/**
 * Created by mknblch on 12.10.2014.
 */
public class SpecialForms {

    @InjectInterpreter
    private static Interpreter interpreter;

    @Define(symbol = "setq", special = true)
    public static Object setq(ListStruct args, Context context) throws Exception {

        final String key = TypeHelper.symbolLiteral(args.car());
        // bind to local but eval args with parent scope
        final Object value = interpreter.eval(args.cdr().car(), context);
        context.bind(key, value);
        return value;
    }

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
            // bind to local but eval args with parent scope
            localScope.bind(TypeHelper.symbolLiteral(pair.car()), interpreter.eval(pair.cdr().car(), env));
        }
        return interpreter.eval(args.cdr().car(), localScope);
    }

    @Define(symbol = "let*", special = true) // (let* ((a 1) (b a)) b) => 1
    public static Object letAsterisk(ListStruct args, Context env) throws Exception {
        final Context localScope = env.derive();
        // car must be list
        for (Object def : (ListStruct) args.car()) {
            // each element must be a symbol-value pair.
            final ListStruct pair = ((ListStruct) def);
            // bind to local and eval with local scope
            localScope.bind(TypeHelper.symbolLiteral(pair.car()), interpreter.eval(pair.cdr().car(), localScope));
        }
        // evaluate cdar with newly build variable scope
        return interpreter.eval(args.cdr().car(), localScope);
    }


    @Define(symbol = "lambda", special = true) // ((lambda (a) (+ a 1)) 1) => 2
    public static Object lambda(final ListStruct pArgs, final Context parentContext) throws Exception {
        // definition scope
        final List<String> symbols = TypeHelper.symbolList(pArgs.car());

        /* evaluation scope */
        return new Function() {

            private final Object func = pArgs.cdr().car();

            @Override
            public Object eval(ListStruct args, Context localContext) throws Exception {
                // bind args to context
                bind(args, localContext, symbols, parentContext);
                // eval with
                return interpreter.eval(func, localContext);
            }

            @Override
            public String getSymbol() {
                return null;
            }

            @Override
            public boolean isSpecialForm() {
                return false;
            }

            @Override
            public Type getType() {
                return Type.FUNC;
            }
        };
    }

    private static void bind(ListStruct args, Context localContext, List<String> symbols, Context parentContext) throws Exception {
        ListStruct temp = args;
        for (int i = 0; i < symbols.size(); i++) {
            if(null == temp) throw new EvaluationException(String.format(
                    "procedure expects %d arguments, given %d", symbols.size(), i));
            localContext.bind(symbols.get(i), interpreter.eval(temp.car(), parentContext));
            temp = temp.cdr();
        }
    }

}

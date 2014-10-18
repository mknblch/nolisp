package de.mknblch.sucode.builtin;

import de.mknblch.sucode.func.*;
import de.mknblch.sucode.helper.TypeHelper;
import de.mknblch.sucode.interpreter.Context;
import de.mknblch.sucode.interpreter.EvaluationException;
import de.mknblch.sucode.interpreter.Interpreter;
import de.mknblch.sucode.ast.ListStruct;

import java.util.List;

/**
 * Created by mknblch on 12.10.2014.
 */
public class SpecialForms {

    @Special
    @Define(symbol = "setq")
    public static Object setq(Interpreter interpreter, Context context, ListStruct args) throws Exception {
        ListStruct temp = args;
        Object value;
        do {
            final String key = TypeHelper.symbolLiteral(temp.car());
            value = interpreter.eval(temp.cdr().car(), context);
            context.bind(key, value);
            temp = temp.cdr().cdr();
        } while (temp != null);

        return value;
    }

    @Special
    @Define(symbol = "quote")
    public static Object quote(Interpreter interpreter, Context context, ListStruct args) throws EvaluationException {
        return args.car();
    }

    @Special
    @Define(symbol = "let") // (let ((a 1) (b a)) b) => ERROR
    public static Object let(Interpreter interpreter, Context parentScope, ListStruct args) throws Exception {
        final Context localScope = parentScope.derive();
        // car must be list
        for (Object def : (ListStruct) args.car()) {
            // each element must be a symbol-value pair.
            final ListStruct pair = ((ListStruct) def);
            // bind to local but eval args with parent scope
            localScope.bind(TypeHelper.symbolLiteral(pair.car()), interpreter.eval(pair.cdr().car(), parentScope));
        }
        return interpreter.eval(args.cdr().car(), localScope);
    }

    @Special
    @Define(symbol = "let*") // (let* ((a 1) (b a)) b) => 1
    public static Object letAsterisk(Interpreter interpreter, Context parentScope, ListStruct args) throws Exception {
        final Context localScope = parentScope.derive();
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


    @Special
    @Define(symbol = "lambda") // ((lambda (a) (+ a 1)) 1) => 2
    public static Object lambda(final Interpreter interpreter, final Context parentContext, final ListStruct pArgs) throws Exception {
        // definition scope
        final List<String> symbols = TypeHelper.symbolList(pArgs.car());

        /* evaluation scope */
        return new NonSpecialForm() {

            @Override
            public Object eval(Context localContext, ListStruct args) throws Exception {
                // bind args to context
                bind(interpreter, parentContext, localContext, symbols, args);
                // eval with
                return interpreter.eval(pArgs.cdr().car(), localContext);
            }

            @Override
            public String getSymbol() {
                return null;
            }
        };
    }

    /**
     * bind each argument in args with key at args index in symbols to the local context by evaluating it with the
     * parent context.
     */
    private static void bind(Interpreter interpreter, Context parentContext, Context localContext, List<String> symbols, ListStruct args) throws Exception {
        ListStruct temp = args;
        for (int i = 0; i < symbols.size(); i++) {
            if(null == temp) throw new EvaluationException(String.format(
                    "procedure expects %d arguments, given %d", symbols.size(), i));
            localContext.bind(symbols.get(i), interpreter.eval(temp.car(), parentContext));
            temp = temp.cdr();
        }
    }

}

package de.mknblch.sucode.interpreter;

import de.mknblch.sucode.interpreter.func.Function;
import de.mknblch.sucode.parser.structs.*;

/**
 * ListStruct Interpreter
 *
 * Created by mknblch on 05.10.2014.
 */
public class Interpreter {

    public static Object eval(Object obj, Context context) throws Exception {
        // null evaluates to null
        if (null == obj) {
            return null;
        }
        // atoms must be evaluated
        if (obj instanceof Atom) {
            final Atom atom = (Atom) obj;
            switch (atom.getType()) {
                case SYMBOL:
                    return context.get(((SymbolStruct) atom).literal);
                case LIST:
                    return evalFunction((ListStruct) atom, context);
                case CONST:
                    return ((ConstStruct) atom).value;
            }
            throw new EvaluationException(String.format("Unknown Atom '%s'.", atom.getType()));
        }
        // non atoms evaluate to itself
        return obj;
    }

    /**
     * evaluates each element in the list without treating the outer list as function call.
     */
    public static ListStruct evalList(ListStruct list, Context context) throws Exception {
        final ListStruct ret = new ListStruct();
        for (Object l : list) {
            ret.add(eval(l, context));
        }
        return ret;
    }

    private static Object evalFunction(ListStruct listStruct, Context context) throws Exception {
        final Object head = eval(listStruct.car(), context);
        if (null == head) {
            throw new EvaluationException(String.format("Procedure application: expected procedure, given: 'nil'."));
        }
        if(!(head instanceof Function)) {
            throw new EvaluationException(String.format("Procedure application: expected procedure, given: '%s'.", head.getClass().getName()));
        }
        final Function function = (Function) head;
        // pre evaluate arguments if no special form
        if(!function.isSpecialForm()) {
            return function.eval(evalList(listStruct.cdr(), context), context);
        }
        return function.eval(listStruct.cdr(), context);
    }

}

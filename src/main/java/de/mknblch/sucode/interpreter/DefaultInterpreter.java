package de.mknblch.sucode.interpreter;

import de.mknblch.sucode.func.Function;
import de.mknblch.sucode.structs.*;

/**
 * ListStruct Interpreter
 *
 * Created by mknblch on 05.10.2014.
 */
public class DefaultInterpreter implements Interpreter {

    @Override
    public Object eval(Object obj, Context context) throws Exception {
        // null evaluates to null
        if (null == obj) {
            return null;
        }
        // non atoms evaluate to itself
        if (!(obj instanceof Atom)) {
            return obj;
        }
        // atoms must be evaluated
        final Atom atom = (Atom) obj;
        switch (atom.getType()) {
            case SYMBOL:
                return context.get(((SymbolStruct) atom).literal);
            case LIST:
                return evalFunction((ListStruct) atom, context);
            case CONST:
                return ((ConstStruct) atom).value;
            default:
                throw new EvaluationException(String.format("Unknown Atom %s:%s", atom, atom.getType()));
        }
    }

    @Override
    public ListStruct evalList(ListStruct list, Context context) throws Exception {
        final ListStruct ret = new ListStruct();
        if (null == list) {
            return null;
        }
        for (Object l : list) {
            if (!(l instanceof Atom)) ret.add(l);
            else ret.add(eval(l, context));
        }
        return ret;
    }

    private Object evalFunction(ListStruct listStruct, Context context) throws Exception {
        final Object head = eval(listStruct.car(), context);
        // verify type
        expectFunction(head);
        final Function function = (Function) head;
        // pre evaluate arguments if no special form
        if(!function.isSpecialForm()) {
            return function.eval(evalList(listStruct.cdr(), context), context);
        }
        return function.eval(listStruct.cdr(), context);
    }

    private static void expectFunction(Object head) throws EvaluationException {
        if (null == head) {
            throw new EvaluationException(String.format("Procedure application: expected procedure, given: nil"));
        }
        if(!(head instanceof Function)) {
            // TODO verify correctness
            throw new EvaluationException(String.format("Procedure application: expected procedure, given: %s:%s", head, head.getClass().getName()));
        }
    }

}

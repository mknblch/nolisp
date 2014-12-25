package de.mknblch.nolisp.interpreter;

import de.mknblch.nolisp.common.TypeHelper;
import de.mknblch.nolisp.datatypes.*;

/**
 * Interpreter
 *
 * @author mknblch
 */
public class CoreInterpreter implements Interpreter {

    @Override
    public Object eval(Object obj, Context context) throws Exception {
        if (!(obj instanceof Atom)) return obj;
        final Atom atom = (Atom) obj;
        switch (atom.getType()) {
            case SYMBOL:
                return context.get(((SymbolStruct) atom).literal);
            case LIST:
                return functionCall((ListStruct) atom, context);
            default:
                throw new EvaluationException(String.format("Unknown Atom %s:%s", atom, atom.getType()));
        }
    }

    @Override
    public ListStruct evalEach(ListStruct list, Context context) throws Exception {
        if (null == list) return null;
        final ListStruct ret = new ListStruct();
        for (Object l : list) {
            if (!(l instanceof Atom)) ret.add(l);
            else ret.add(eval(l, context));
        }
        return ret;
    }

    private Object functionCall(ListStruct list, Context context) throws Exception {
        if(TypeHelper.isEmptyList(list)) return null;
        final Object func = eval(list.car(), context);
        if (null == func) {
            throw new EvaluationException(String.format("Procedure application: expected procedure, given: nil"));
        } else if (func instanceof Form) {
            return ((Form) func).eval(evalEach(list.cdr(), context));
        } else if (func instanceof SpecialForm) {
            return ((SpecialForm) func).eval(this, context, list.cdr());
        } else {
            throw new EvaluationException(String.format(
                    "Procedure application: expected procedure, given: %s:%s",
                    func,
                    func.getClass().getName()));
        }
    }

}

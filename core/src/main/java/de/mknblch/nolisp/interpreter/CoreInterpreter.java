package de.mknblch.nolisp.interpreter;

import de.mknblch.nolisp.common.TypeHelper;
import de.mknblch.nolisp.datatypes.Atom;
import de.mknblch.nolisp.datatypes.ListStruct;
import de.mknblch.nolisp.datatypes.SymbolStruct;
import de.mknblch.nolisp.datatypes.forms.Form;
import de.mknblch.nolisp.datatypes.forms.SpecialForm;

/**
 * Interpreter
 *
 * @author mknblch
 */
public class CoreInterpreter implements Interpreter {

    @Override
    public Object eval(Object obj, Context context) throws Exception {
        // non atoms evaluate to itself
        if (!(obj instanceof Atom)) return obj;
        // atoms must be evaluated
        final Atom atom = (Atom) obj;
        switch (atom.getType()) {
            case SYMBOL:
                return retrieveFromContext((SymbolStruct) atom, context);
            case LIST:
                return functionCall((ListStruct) atom, context);

            case BUILTIN:
            case MACRO:
            case LAMBDA:
                return obj;

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

    private Object retrieveFromContext(SymbolStruct atom, Context context) throws EvaluationException {
        return context.get(atom.literal);
    }

    private Object functionCall(ListStruct list, Context context) throws Exception {
        if(TypeHelper.isEmptyList(list)) return null;
        // retrieve the function at list.car from context
        final Object func = eval(list.car(), context);
        if (null == func) {
            throw new EvaluationException(String.format("Procedure application: expected procedure, given: nil"));
        } else if (func instanceof Form) {
            // each argument is evaluated before function call
            return ((Form) func).eval(evalEach(list.cdr(), context));
        } else if (func instanceof SpecialForm) {
            // specialForms get the pure AST
            return ((SpecialForm) func).eval(this, context, list.cdr());
        } else {
            throw new EvaluationException(String.format("Procedure application: expected procedure, given: %s:%s", func, func.getClass().getName()));
        }
    }

}
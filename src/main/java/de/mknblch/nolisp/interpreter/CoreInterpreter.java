package de.mknblch.nolisp.interpreter;

import de.mknblch.nolisp.ast.Atom;
import de.mknblch.nolisp.ast.ListStruct;
import de.mknblch.nolisp.ast.SymbolStruct;
import de.mknblch.nolisp.ast.forms.Form;
import de.mknblch.nolisp.ast.forms.SpecialForm;

/**
 * ListStruct Interpreter
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
            case LAMBDA:
            case MACRO:
                return obj;

            default:
                throw new EvaluationException(String.format("Unknown Atom %s:%s", atom, atom.getType()));
        }
    }

    @Override
    public ListStruct evalEach(ListStruct list, Context context) throws Exception {
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

    private Object retrieveFromContext(SymbolStruct atom, Context context) throws EvaluationException {
        return context.get(((SymbolStruct) atom).literal);
    }

    private Object functionCall(ListStruct list, Context context) throws Exception {
        // retrieve the function at list.car from context
        final Object func = eval(list.car(), context);
        if (null == func) {
            throw new EvaluationException(String.format("Procedure application: expected procedure, given: nil"));
        } else if (func instanceof Form) {
            // arguments of Forms are executed before function call
            return ((Form) func).eval(context, evalEach(list.cdr(), context));
        } else if (func instanceof SpecialForm) {
            // arguments of BasicForms will NOT become evaluated before call
            return ((SpecialForm) func).eval(this, context, list.cdr());
        } else {
            throw new EvaluationException(String.format("Procedure application: expected procedure, given: %s:%s", func, func.getClass().getName()));
        }
    }

}

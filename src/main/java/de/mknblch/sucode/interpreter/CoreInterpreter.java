package de.mknblch.sucode.interpreter;

import de.mknblch.sucode.ast.Atom;
import de.mknblch.sucode.ast.ListStruct;
import de.mknblch.sucode.ast.Reference;
import de.mknblch.sucode.ast.SymbolStruct;
import de.mknblch.sucode.ast.forms.Form;
import de.mknblch.sucode.ast.forms.Function;
import de.mknblch.sucode.ast.forms.SpecialForm;

import static de.mknblch.sucode.helper.TypeHelper.asFunction;

/**
 * ListStruct Interpreter
 *
 * @author mknblch
 */
public class CoreInterpreter implements Interpreter {

    @Override
    public Object eval(Object obj, Context context) throws Exception {
        // null evaluates to null
        if (null == obj) return null;
        // non atoms evaluate to itself
        if (!(obj instanceof Atom)) return obj;
        // atoms must be evaluated
        final Atom atom = (Atom) obj;
        switch (atom.getType()) {
            case SYMBOL:
                return retrieveFromContext(context, (SymbolStruct) atom);
            case LIST:
                return evalList((ListStruct) atom, context);

            // TODO review
            case FORM:
            case LAMBDA:
                return obj;

            default:
                throw new EvaluationException(String.format("Unknown Atom %s:%s", atom, atom.getType()));
        }
    }

    private Object retrieveFromContext(Context context, SymbolStruct atom) throws EvaluationException {
        Object ret = context.get(((SymbolStruct) atom).literal);
        while(ret instanceof Reference) {
            ret = context.get(((Reference) ret).target);
        }
        return ret;
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

    private Object evalList(ListStruct list, Context context) throws Exception {
        // expect a target pointing to the actual function implementation
        final Object head = list.car();
        // retrieve the function o from context
        final Function func = asFunction(eval(head, context));
        // eval in current context
        return evalFunction(func, context, list);
    }

    private Object evalFunction(Function function, Context context, ListStruct args) throws Exception {
        if (null == function) {
            throw new EvaluationException(String.format("Procedure application: expected procedure, given: nil"));
        }
        if (function instanceof Form) {
            // arguments of Forms are executed before function call
            return ((Form) function).eval(context, evalEach(args.cdr(), context));
        } else if (function instanceof SpecialForm) {
            // arguments of SpecialForms will NOT become evaluated before call
            return ((SpecialForm) function).eval(this, context, args.cdr());

        } else {
            throw new EvaluationException(String.format("Procedure application: expected procedure, given: %s:%s", function, function.getClass().getName()));
        }
    }
}

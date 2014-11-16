package de.mknblch.nolisp.core.interpreter;

import de.mknblch.nolisp.core.interpreter.structs.Atom;
import de.mknblch.nolisp.core.interpreter.structs.ListStruct;
import de.mknblch.nolisp.core.interpreter.structs.SymbolStruct;
import de.mknblch.nolisp.core.interpreter.structs.forms.Form;
import de.mknblch.nolisp.core.interpreter.structs.forms.SpecialForm;
import de.mknblch.nolisp.core.interpreter.parser.Parser;

/**
 * ListStruct Interpreter
 *
 * @author mknblch
 */
public class CoreInterpreter implements Interpreter {

    private final Language language;
    protected final Parser parser;

    public CoreInterpreter(Language language) {
        this.language = language;
        this.parser = new Parser();
    }

    @Override
    public Language getLanguage() {
        return language;
    }

    @Override
    public Object eval(String code) throws Exception {
        final Context context = new Context(language);
        return evalEach(parser.parse(code), context);
    }

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
        // retrieve the function at list.car from context
        final Object func = eval(list.car(), context);
        if (null == func) {
            throw new EvaluationException(String.format("Procedure application: expected procedure, given: nil"));
        } else if (func instanceof Form) {
            // arguments of Forms are executed before function call
            return ((Form) func).eval(context, evalEach(list.cdr(), context));
        } else if (func instanceof SpecialForm) {
            // arguments of SpecialForms will NOT become evaluated before call
            return ((SpecialForm) func).eval(this, context, list.cdr());
        } else {
            throw new EvaluationException(String.format("Procedure application: expected procedure, given: %s:%s", func, func.getClass().getName()));
        }
    }

}

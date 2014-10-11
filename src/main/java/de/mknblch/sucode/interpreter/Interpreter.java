package de.mknblch.sucode.interpreter;

import de.mknblch.sucode.interpreter.forms.FormRegister;
import de.mknblch.sucode.parser.structs.*;

/**
 * Created by mknblch on 05.10.2014.
 */
public class Interpreter {

    private final FormRegister formRegister;

    public Interpreter(FormRegister formRegister) {
        this.formRegister = formRegister;
    }

    public FormRegister getFormRegister() {
        return formRegister;
    }

    public Object eval(Object obj, Environment environment) throws EvaluationException {
        // null evaluates to null
        if (null == obj) {
            return null;
        }
        // atoms must be evaluated
        if (obj instanceof Atom) {
            final Atom atom = (Atom) obj;
            switch (atom.getType()) {
                case SYMBOL:
                    return eval(environment.get(((SymbolStruct) atom).literal), environment);
                case LIST:
                    return formRegister.getForm(symbolValue(((ListStruct) atom).car())).eval(((ListStruct) atom).cdr(), environment, this);
                case CONST:
                    return ((ConstStruct) atom).value;
            }
            throw new EvaluationException(String.format("Unknown Atom '%s'.", atom.getType()));
        }
        // non atoms evaluate to itself
        return obj;
    }

    private static String symbolValue (Object obj) throws EvaluationException {
        if(!(obj instanceof SymbolStruct)) {
            throw new EvaluationException(String.format("Cannot retrieve symbol-value of type %s", obj.getClass().getName()));
        }
        return ((SymbolStruct)obj).literal;
    }
}

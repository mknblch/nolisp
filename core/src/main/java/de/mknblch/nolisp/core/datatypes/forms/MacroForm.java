package de.mknblch.nolisp.core.datatypes.forms;

import de.mknblch.nolisp.core.interpreter.Context;
import de.mknblch.nolisp.core.interpreter.EvaluationException;
import de.mknblch.nolisp.core.interpreter.Interpreter;
import de.mknblch.nolisp.core.datatypes.Atom;
import de.mknblch.nolisp.core.datatypes.ListStruct;

import java.util.List;

/**
 * (defmacro target (arg*) from+)
 *
 * @author mknblch
 */
public class MacroForm implements SpecialForm {

    private final ListStruct forms;
    private final List<String> formSymbols;

    public MacroForm(List<String> argumentSymbols, ListStruct forms) {
        this.formSymbols = argumentSymbols;
        this.forms = forms;
    }

    @Override
    public Object eval(Interpreter interpreter, Context context, ListStruct args) throws Exception {
        bind(context, formSymbols, args); // TODO review: correct context?
        final ListStruct expanded = expand(interpreter, context);
        Object ret = null;
        for (Object o : expanded) {
            ret = interpreter.eval(o, context);
        }
        return ret;
    }

    private ListStruct expand(Interpreter interpreter, Context context) throws Exception {
        // expand the macro to it's actual form
        return interpreter.evalEach(forms, context); // TODO review: correct context?
    }

    @Override
    public Atom.Type getType() {
        return Atom.Type.MACRO;
    }

    public List<String> getArgumentSymbols() {
        return formSymbols;
    }

    public Object getForms() {
        return forms;
    }

    /**
     * bind each argument to the symbol associated to its position
     */
    private static void bind(Context context, List<String> symbols, ListStruct values) throws Exception {

        ListStruct temp = values;
        for (int i = 0; i < symbols.size(); i++) {
            if (null == temp) {
                throw new EvaluationException(String.format(
                        "procedure expects %d arguments, given %d", symbols.size(), i));
            }
            context.bind(symbols.get(i), temp.car());
            temp = temp.cdr();
        }
        if (null != temp) {
            throw new EvaluationException(String.format(
                    "procedure expects %d arguments, given %d", symbols.size(), values.size()));
        }
    }
}

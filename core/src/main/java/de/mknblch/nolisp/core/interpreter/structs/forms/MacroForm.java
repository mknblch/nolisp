package de.mknblch.nolisp.core.interpreter.structs.forms;

import de.mknblch.nolisp.core.interpreter.structs.ListStruct;
import de.mknblch.nolisp.core.interpreter.structs.Atom;
import de.mknblch.nolisp.core.interpreter.Context;
import de.mknblch.nolisp.core.interpreter.EvaluationException;
import de.mknblch.nolisp.core.interpreter.Interpreter;

import java.util.List;

/**
 * (defmacro target (arg*) from+)
 *
 * @author mknblch
 */
public class MacroForm implements SpecialForm {

    private final ListStruct forms;
    private final List<String> formSymbols;

    public MacroForm(List<String> formSymbols, ListStruct forms) {
        this.formSymbols = formSymbols;
        this.forms = forms;
    }

    @Override // args=(arg1 arg2 ...)
    public Object eval(Interpreter interpreter, Context context, ListStruct args) throws Exception {
        bind(context, formSymbols, args);
        Object ret = null;
        for (Object o : forms) {
            final Object eval = interpreter.eval(o, context); //
            ret = interpreter.eval(eval, context);
        }
        return ret;
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

    private static void bind(Context context,
                             List<String> symbols,
                             ListStruct args) throws Exception {

        ListStruct temp = args;
        for (int i = 0; i < symbols.size(); i++) {
            if(null == temp) {
                throw new EvaluationException(String.format(
                        "macro expects %d arguments, given %d", symbols.size(), i));
            }
            context.bind(symbols.get(i), temp.car());
            temp = temp.cdr();
        }
    }
}

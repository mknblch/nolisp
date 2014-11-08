package de.mknblch.nolisp.ast.forms;

import de.mknblch.nolisp.ast.ListStruct;
import de.mknblch.nolisp.helper.FormatHelper;
import de.mknblch.nolisp.interpreter.Context;
import de.mknblch.nolisp.interpreter.EvaluationException;
import de.mknblch.nolisp.interpreter.Interpreter;

import java.util.List;

/**
 * (defmacro target (arg*) from+)
 *
 * @author mknblch
 */
public class MacroForm extends SpecialForm {

    private final String symbol;
    private final ListStruct forms;
    private final List<String> formSymbols;

    // TODO remove symbol
    public MacroForm(String symbol, List<String> formSymbols, ListStruct forms) {
        this.symbol = symbol;
        this.formSymbols = formSymbols;
        this.forms = forms;
    }

    @Override // args=(arg1 arg2 ...)
    public Object eval(Interpreter interpreter, Context context, ListStruct args) throws Exception {
        bind(context, formSymbols, args);
        Object ret = null;
        for (Object o : forms) {
            final Object eval = interpreter.eval(o, context);
            ret = interpreter.eval(eval, context);

        }
        return ret;
    }

    @Override
    public String getSymbol() {
        return symbol;
    }

    @Override
    public Type getType() {
        return Type.MACRO;
    }


    public List<String> getFormSymbols() {
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
                        "procedure expects %d arguments, given %d", symbols.size(), i));
            }
            context.bind(symbols.get(i), temp.car());
            temp = temp.cdr();
        }
    }
}

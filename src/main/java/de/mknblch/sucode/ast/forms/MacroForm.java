package de.mknblch.sucode.ast.forms;

import de.mknblch.sucode.ast.ListStruct;
import de.mknblch.sucode.ast.SymbolStruct;
import de.mknblch.sucode.helper.FormatHelper;
import de.mknblch.sucode.helper.TypeHelper;
import de.mknblch.sucode.interpreter.Context;
import de.mknblch.sucode.interpreter.EvaluationException;
import de.mknblch.sucode.interpreter.Interpreter;

import java.util.ArrayList;
import java.util.List;

import static de.mknblch.sucode.helper.TypeHelper.*;

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
        System.out.printf("evaluating macro: %s%n", FormatHelper.formatPretty(forms));
        bind(context, formSymbols, args);
        Object ret = null;
        for (Object o : forms) {
            final Object eval = interpreter.eval(o, context);
            System.out.printf("macroform: %s ==> %s%n", FormatHelper.formatPretty(o), FormatHelper.formatPretty(eval));

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

    /**
     * bind each argument in args with key at args index in symbols to the local context by evaluating it with the
     * parent context.
     */
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

package de.mknblch.sucode.ast.forms;

import de.mknblch.sucode.ast.ListStruct;
import de.mknblch.sucode.interpreter.Context;
import de.mknblch.sucode.interpreter.EvaluationException;
import de.mknblch.sucode.interpreter.Interpreter;

import java.util.List;

/**
 *
 *
 * (defmacro target (arg*) from+)
 *
 * @author mknblch
 */
public class MacroForm extends SpecialForm {

    private final String symbol;
    private final List<String> formSymbols;
    private final ListStruct form;

    // (defmacro symbol (args*) from+)
    public MacroForm(String symbol, List<String> formSymbols, ListStruct form) {
        this.symbol = symbol;
        this.formSymbols = formSymbols;
        this.form = form;
    }

    @Override // args=(arg1 arg2 ...)
    public Object eval(Interpreter interpreter, Context localContext, ListStruct args) throws Exception {
        // bind args to context
//        bind(interpreter, definitionScopeContext, localContext, formSymbols, args);



        // eval with local
        return interpreter.eval(form, localContext);
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

    public Object getForm() {
        return form;
    }

    /**
     * bind each argument in args with key at args index in formSymbols to the local context by evaluating it with the
     * parent context.
     */
    private static void bind(Interpreter interpreter,
                             Context parentContext,
                             Context localContext,
                             List<String> symbols,
                             ListStruct args) throws Exception {

        ListStruct temp = args;
        for (int i = 0; i < symbols.size(); i++) {
            if(null == temp) {
                throw new EvaluationException(String.format(
                        "procedure expects %d arguments, given %d", symbols.size(), i));
            }
            localContext.bind(symbols.get(i), interpreter.eval(temp.car(), parentContext));
            temp = temp.cdr();
        }
    }
}

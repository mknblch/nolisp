package de.mknblch.nolisp.ast.forms;

import de.mknblch.nolisp.ast.ListStruct;
import de.mknblch.nolisp.interpreter.Context;
import de.mknblch.nolisp.interpreter.EvaluationException;
import de.mknblch.nolisp.interpreter.Interpreter;

import java.util.List;

/**
 * @author mknblch
 */
public class LambdaForm extends Form {
    private final Interpreter interpreter;
    private final Context definitionScopeContext;
    private final List<String> symbols;
    private final Object form;

    public LambdaForm(Interpreter interpreter, Context definitionScopeContext, List<String> formSymbols, Object form) {
        this.interpreter = interpreter;
        this.definitionScopeContext = definitionScopeContext;
        this.symbols = formSymbols;
        this.form = form;
    }

    @Override
    public Object eval(Context localContext, ListStruct args) throws Exception {
        // bind args to context
        bind(interpreter, definitionScopeContext, localContext, symbols, args);
        // eval with local
        return interpreter.eval(form, localContext);
    }

    @Override
    public String getSymbol() {
        return null;
    }

    @Override
    public Type getType() {
        return Type.LAMBDA;
    }

    public List<String> getSymbols() {
        return symbols;
    }

    public Object getForm() {
        return form;
    }

    /**
     * bind each argument in args with key at args index in symbols to the local context by evaluating it in the
     * definition scope context. (TODO REVIEW)
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

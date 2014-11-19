package de.mknblch.nolisp.core.interpreter.structs.forms;

import de.mknblch.nolisp.core.interpreter.Context;
import de.mknblch.nolisp.core.interpreter.EvaluationException;
import de.mknblch.nolisp.core.interpreter.Interpreter;
import de.mknblch.nolisp.core.interpreter.structs.Atom;
import de.mknblch.nolisp.core.interpreter.structs.ListStruct;

import java.util.List;

/**
 * @author mknblch
 */
public class LambdaForm implements Form {
    private final Interpreter interpreter;
    private final List<String> symbols;
    private final Object form;
    private final Context context;

    public LambdaForm(Interpreter interpreter, Context context, List<String> formSymbols, Object form) {
        this.interpreter = interpreter;
        this.context = context;
        this.symbols = formSymbols;
        this.form = form;
    }

    @Override
    public Object eval(ListStruct args) throws Exception {
        // derive local scope context
        final Context executionContext = context.derive();
        // bind this to context
        executionContext.bind("this", this);
        // bind args to context
        bind(executionContext, symbols, args);
        // eval
        return interpreter.eval(form, executionContext);
    }

    @Override
    public Atom.Type getType() {
        return Atom.Type.LAMBDA;
    }

    public List<String> getArgumentSymbols() {
        return symbols;
    }

    public Object getForm() {
        return form;
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

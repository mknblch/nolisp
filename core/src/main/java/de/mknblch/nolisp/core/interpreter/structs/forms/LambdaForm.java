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

    public LambdaForm(Interpreter interpreter, List<String> formSymbols, Object form) {
        this.interpreter = interpreter;
        this.symbols = formSymbols;
        this.form = form;
    }

    @Override
    public Object eval(Context context, ListStruct args) throws Exception {
        final Context localContext = context.derive(); // TODO move to definition scope ??
        // bind args to context
        bind(interpreter, context, localContext, symbols, args);
        // bind this
        localContext.bind("this", this);
        // eval with local
        return interpreter.eval(form, localContext);
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
            if (null == temp) {
                throw new EvaluationException(String.format(
                        "procedure expects %d arguments, given %d", symbols.size(), i));
            }
            localContext.bind(symbols.get(i), interpreter.eval(temp.car(), parentContext));
            temp = temp.cdr();
        }
    }
}

package de.mknblch.nolisp.core.interpreter.structs.forms;

import de.mknblch.nolisp.core.common.Expectations;
import de.mknblch.nolisp.core.common.FormatHelper;
import de.mknblch.nolisp.core.common.TypeHelper;
import de.mknblch.nolisp.core.interpreter.Context;
import de.mknblch.nolisp.core.interpreter.EvaluationException;
import de.mknblch.nolisp.core.interpreter.Interpreter;
import de.mknblch.nolisp.core.interpreter.structs.Atom;
import de.mknblch.nolisp.core.interpreter.structs.ListStruct;
import de.mknblch.nolisp.core.interpreter.structs.SymbolStruct;

import java.util.Iterator;
import java.util.List;

/**
 * @author mknblch
 */
public class LambdaForm implements Form {
    private final Interpreter interpreter;
    private final ListStruct symbols;
    private final Object form;
    private final Context context;

    public LambdaForm(Interpreter interpreter, Context context, ListStruct symbols, Object form) {
        this.interpreter = interpreter;
        this.context = context;
        this.symbols = symbols;
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

    public ListStruct getArgumentSymbols() {
        return symbols;
    }

    public Object getForm() {
        return form;
    }

    /**
     * bind each argument to the symbol associated to its position
     */
    private static void bind(Context context, ListStruct symbols, ListStruct values) throws Exception {

        // TODO review values = null
        for (Object symbol : symbols) {
            if (null == symbol) {
                return;
            }
            if (null == values) {
                throw new EvaluationException(String.format(
                        "procedure expects %d arguments, given: nil", symbols.size()));
            }
            context.bind(TypeHelper.symbolLiteral(symbol), values.car());
            values = values.cdr();
        }
    }
}

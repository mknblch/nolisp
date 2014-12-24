package de.mknblch.nolisp.features.lambda;

import de.mknblch.nolisp.common.FormatHelper;
import de.mknblch.nolisp.common.TypeHelper;
import de.mknblch.nolisp.datatypes.Form;
import de.mknblch.nolisp.interpreter.Context;
import de.mknblch.nolisp.interpreter.EvaluationException;
import de.mknblch.nolisp.interpreter.Interpreter;
import de.mknblch.nolisp.datatypes.Atom;
import de.mknblch.nolisp.datatypes.ListStruct;

/**
 * @author mknblch
 */
public class Lambda implements Form {

    private final Interpreter interpreter;
    private final ListStruct symbols;
    private final Object form;
    private final Context context;

    public Lambda(Interpreter interpreter, Context context, ListStruct symbols, Object form) {
        this.interpreter = interpreter;
        this.context = context;
        this.symbols = symbols;
        this.form = form;
    }

    @Override
    public Object eval(ListStruct args) throws Exception {
        // derive local scope context
        final Context executionContext = context.derive();
        // bind args to context
        bind(executionContext, symbols, args);
        // eval
        return interpreter.eval(form, executionContext);
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
        for (Object symbol : symbols) {
            if (null == symbol) {
                return;
            }
            if (null == values) {
                throw new EvaluationException(String.format(
                        "procedure expects %d arguments, given: nil", symbols.size()));
            }
            context.bind(TypeHelper.getSymbolLiteral(symbol), values.car());
            values = values.cdr();
        }
    }

    public static Lambda asLambda(Object o) throws EvaluationException {
        if (!(o instanceof Lambda)) {
            throw new EvaluationException(
                    String.format("Expected FORM but was: %s", FormatHelper.formatAtom(o)));
        }
        return (Lambda) o;
    }
}

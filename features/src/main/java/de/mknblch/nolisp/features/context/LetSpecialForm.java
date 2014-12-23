package de.mknblch.nolisp.features.context;

import de.mknblch.nolisp.common.Expectations;
import de.mknblch.nolisp.common.TypeHelper;
import de.mknblch.nolisp.datatypes.ListStruct;
import de.mknblch.nolisp.datatypes.builtin.BuiltInSpecialForm;
import de.mknblch.nolisp.interpreter.Context;
import de.mknblch.nolisp.interpreter.Interpreter;
import de.mknblch.nolisp.generator.Define;

import static de.mknblch.nolisp.common.TypeHelper.asList;
import static de.mknblch.nolisp.common.TypeHelper.getSymbolLiteral;

/**
 * @author mknblch
 */
@Define({"let"})
public class LetSpecialForm extends BuiltInSpecialForm  {

    @Override
    public Object eval(Interpreter interpreter, Context context, ListStruct args) throws Exception {
        final Context localScope = context.derive();
        // car must be list
        final ListStruct definitions = TypeHelper.asList(args.car());
        for (Object def : definitions) {
            // each element must be a value-value pair.
            final ListStruct pair = TypeHelper.asList(def);
            Expectations.expectCdr(pair);
            // bind to local but eval args with parent scope
            localScope.bind(TypeHelper.getSymbolLiteral(pair.car()), interpreter.eval(pair.cdr().car(), context));
        }
        return interpreter.eval(args.cdr().car(), localScope);
    }
}
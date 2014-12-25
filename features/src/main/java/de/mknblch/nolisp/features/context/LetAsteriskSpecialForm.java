package de.mknblch.nolisp.features.context;

import de.mknblch.nolisp.common.Expectations;
import de.mknblch.nolisp.common.TypeHelper;
import de.mknblch.nolisp.datatypes.ListStruct;
import de.mknblch.nolisp.datatypes.SpecialForm;
import de.mknblch.nolisp.generator.Define;
import de.mknblch.nolisp.interpreter.Context;
import de.mknblch.nolisp.interpreter.Interpreter;

/**
 * @author mknblch
 */
@Define({"let*"})
public class LetAsteriskSpecialForm implements SpecialForm {

    @Override
    public Object eval(Interpreter interpreter, Context context, ListStruct args) throws Exception {
        final Context localScope = context.derive();
        final ListStruct definitions = TypeHelper.asList(args.car());
        for (Object def : definitions) {
            // each element must be a key-value pair (2 element list).
            final ListStruct pair = TypeHelper.asList(def);
            // lst must have a successor
            Expectations.expectCdr(pair);
            // bind to local and eval with local scope
            localScope.bind(TypeHelper.getSymbolLiteral(pair.car()), interpreter.eval(pair.cdr().car(), localScope));
        }
        // evaluate cdar with newly build variable scope
        return interpreter.eval(args.cdr().car(), localScope);
    }
}
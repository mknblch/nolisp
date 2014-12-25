package de.mknblch.nolisp.features.context;

import de.mknblch.nolisp.common.TypeHelper;
import de.mknblch.nolisp.datatypes.ListStruct;
import de.mknblch.nolisp.datatypes.SpecialForm;
import de.mknblch.nolisp.generator.Define;
import de.mknblch.nolisp.interpreter.Context;
import de.mknblch.nolisp.interpreter.Interpreter;

/**
 * @author mknblch
 */
@Define({"isset?"})
public class IsVarSetSpecialForm implements SpecialForm {

    @Override
    public Object eval(Interpreter interpreter, Context context, ListStruct args) throws Exception {
        return context.containsKey(TypeHelper.getSymbolLiteral(args.car()));
    }
}
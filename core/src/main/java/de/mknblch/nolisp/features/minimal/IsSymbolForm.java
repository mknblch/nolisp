package de.mknblch.nolisp.features.minimal;

import de.mknblch.nolisp.common.TypeHelper;
import de.mknblch.nolisp.datatypes.ListStruct;
import de.mknblch.nolisp.dialect.builtin.BuiltInForm;
import de.mknblch.nolisp.codegen.Define;

/**
 * @author mknblch
 */
@Define({"symbol?"})
public class IsSymbolForm extends BuiltInForm  {

    @Override
    public Object eval(ListStruct args) throws Exception {
        return TypeHelper.isSymbol(args.car());
    }
}    
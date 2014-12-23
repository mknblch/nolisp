package de.mknblch.nolisp.features.number;

import de.mknblch.nolisp.datatypes.ListStruct;
import de.mknblch.nolisp.dialect.builtin.BuiltInForm;
import de.mknblch.nolisp.codegen.Define;

/**
 * @author mknblch
 */
@Define({"int?"})
public class IsIntForm extends BuiltInForm  {

    @Override
    public Object eval(ListStruct args) throws Exception {
        return args.car() instanceof Integer;
    }
}    
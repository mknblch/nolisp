package de.mknblch.nolisp.features.math;

import de.mknblch.nolisp.common.TypeHelper;
import de.mknblch.nolisp.datatypes.ListStruct;
import de.mknblch.nolisp.dialect.builtin.BuiltInForm;
import de.mknblch.nolisp.codegen.Define;

/**
 * @author mknblch
 */
@Define({"tan"})
public class TanForm extends BuiltInForm  {

    @Override
    public Object eval(ListStruct args) throws Exception {
        return Math.tan(TypeHelper.asReal(args.car()));
    }
}    
package de.mknblch.nolisp.features.math;

import de.mknblch.nolisp.common.TypeHelper;
import de.mknblch.nolisp.datatypes.ListStruct;
import de.mknblch.nolisp.dialect.builtin.BuiltInForm;
import de.mknblch.nolisp.dialect.Define;

/**
 * @author mknblch
 */
@Define({"sin"})
public class SinForm extends BuiltInForm  {

    @Override
    public Object eval(ListStruct args) throws Exception {
        return Math.sin(TypeHelper.asReal(args.car()));
    }
}    
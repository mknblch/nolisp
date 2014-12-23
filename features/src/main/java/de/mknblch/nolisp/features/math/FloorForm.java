package de.mknblch.nolisp.features.math;

import de.mknblch.nolisp.common.TypeHelper;
import de.mknblch.nolisp.datatypes.ListStruct;
import de.mknblch.nolisp.datatypes.builtin.BuiltInForm;
import de.mknblch.nolisp.generator.Define;

/**
 * @author mknblch
 */
@Define({"floor"})
public class FloorForm extends BuiltInForm  {

    @Override
    public Object eval(ListStruct args) throws Exception {
        return Math.floor(TypeHelper.asReal(args.car()));
    }
}    
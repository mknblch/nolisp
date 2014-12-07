package de.mknblch.nolisp.features.math;

import de.mknblch.nolisp.common.TypeHelper;
import de.mknblch.nolisp.datatypes.ListStruct;
import de.mknblch.nolisp.datatypes.builtin.BuiltInForm;

/**
 * @author mknblch
 */
public class FloorForm extends BuiltInForm {

    @Override
    public String[] getSymbols() {
        return new String[]{"floor"};
    }

    @Override
    public Object eval(ListStruct args) throws Exception {
        return Math.floor(TypeHelper.asReal(args.car()));
    }
}    
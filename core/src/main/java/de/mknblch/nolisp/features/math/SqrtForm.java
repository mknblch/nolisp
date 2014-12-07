package de.mknblch.nolisp.features.math;

import de.mknblch.nolisp.common.TypeHelper;
import de.mknblch.nolisp.datatypes.ListStruct;
import de.mknblch.nolisp.datatypes.builtin.BuiltInForm;

/**
 * @author mknblch
 */
public class SqrtForm extends BuiltInForm {

    @Override
    public String[] getSymbols() {
        return new String[]{"sqrt"};
    }

    @Override
    public Object eval(ListStruct args) throws Exception {
        return Math.sqrt(TypeHelper.asReal(args.car()));
    }
}    
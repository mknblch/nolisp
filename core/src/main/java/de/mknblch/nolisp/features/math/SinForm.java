package de.mknblch.nolisp.features.math;

import de.mknblch.nolisp.common.TypeHelper;
import de.mknblch.nolisp.datatypes.ListStruct;
import de.mknblch.nolisp.datatypes.builtin.BuiltInForm;

/**
 * @author mknblch
 */
public class SinForm extends BuiltInForm {

    @Override
    public String[] getSymbols() {
        return new String[]{"sin"};
    }

    @Override
    public Object eval(ListStruct args) throws Exception {
        return Math.sin(TypeHelper.asReal(args.car()));
    }
}    
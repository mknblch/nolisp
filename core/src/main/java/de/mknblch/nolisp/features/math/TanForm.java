package de.mknblch.nolisp.features.math;

import de.mknblch.nolisp.common.TypeHelper;
import de.mknblch.nolisp.datatypes.ListStruct;
import de.mknblch.nolisp.datatypes.forms.BuiltInForm;

/**
 * @author mknblch
 */
public class TanForm extends BuiltInForm {

    @Override
    public String[] getSymbols() {
        return new String[]{"tan"};
    }

    @Override
    public Object eval(ListStruct args) throws Exception {
        return Math.tan(TypeHelper.asReal(args.car()));
    }
}    
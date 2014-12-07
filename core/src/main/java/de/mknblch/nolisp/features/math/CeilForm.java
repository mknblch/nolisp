package de.mknblch.nolisp.features.math;

import de.mknblch.nolisp.common.TypeHelper;
import de.mknblch.nolisp.datatypes.ListStruct;
import de.mknblch.nolisp.datatypes.forms.BuiltInForm;

/**
 * @author mknblch
 */
public class CeilForm extends BuiltInForm {

    @Override
    public String[] getSymbols() {
        return new String[]{"ceil"};
    }

    @Override
    public Object eval(ListStruct args) throws Exception {
        return Math.ceil(TypeHelper.asReal(args.car()));
    }
}    
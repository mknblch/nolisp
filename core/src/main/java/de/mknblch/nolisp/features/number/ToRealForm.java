package de.mknblch.nolisp.features.number;

import de.mknblch.nolisp.common.TypeHelper;
import de.mknblch.nolisp.datatypes.ListStruct;
import de.mknblch.nolisp.datatypes.builtin.BuiltInForm;

/**
 * @author mknblch
 */
public class ToRealForm extends BuiltInForm {

    @Override
    public String[] getSymbols() {
        return new String[]{"toreal"};
    }

    @Override
    public Object eval(ListStruct args) throws Exception {
        return TypeHelper.asReal(args.car());
    }
}    
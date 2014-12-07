package de.mknblch.nolisp.features.java;

import de.mknblch.nolisp.datatypes.ListStruct;
import de.mknblch.nolisp.datatypes.builtin.BuiltInForm;

import static de.mknblch.nolisp.common.TypeHelper.asException;

/**
 * @author mknblch
 */
public class ThrowForm extends BuiltInForm {

    @Override
    public String[] getSymbols() {
        return new String[]{"throw"};
    }

    @Override
    public Object eval(ListStruct args) throws Exception {
        throw asException(args.car());
    }
}    
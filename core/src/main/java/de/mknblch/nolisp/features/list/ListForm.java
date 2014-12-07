package de.mknblch.nolisp.features.list;

import de.mknblch.nolisp.datatypes.ListStruct;
import de.mknblch.nolisp.datatypes.builtin.BuiltInForm;

/**
 * @author mknblch
 */
public class ListForm extends BuiltInForm {

    @Override
    public String[] getSymbols() {
        return new String[]{"list"};
    }

    @Override
    public Object eval(ListStruct args) throws Exception {
        return args;
    }
}    
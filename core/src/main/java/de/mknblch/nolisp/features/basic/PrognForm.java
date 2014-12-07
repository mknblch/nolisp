package de.mknblch.nolisp.features.basic;

import de.mknblch.nolisp.datatypes.ListStruct;
import de.mknblch.nolisp.datatypes.builtin.BuiltInForm;

/**
 * @author mknblch
 */
public class PrognForm extends BuiltInForm {
    @Override
    public String[] getSymbols() {
        return new String[]{"progn"};
    }

    @Override
    public Object eval(ListStruct args) throws Exception {
        if (null == args) {
            return null;
        }
        return args.last().car();
    }
}    
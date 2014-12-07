package de.mknblch.nolisp.features.logic;

import de.mknblch.nolisp.common.TypeHelper;
import de.mknblch.nolisp.datatypes.ListStruct;
import de.mknblch.nolisp.datatypes.builtin.BuiltInForm;

/**
 * @author mknblch
 */
public class AndForm extends BuiltInForm {

    @Override
    public String[] getSymbols() {
        return new String[]{"and"};
    }

    @Override
    public Object eval(ListStruct args) throws Exception {
        for (Object arg : args) {
            if (!TypeHelper.asBoolean(arg)) return false;
        }
        return true;
    }
}    
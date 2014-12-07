package de.mknblch.nolisp.features.logic;

import de.mknblch.nolisp.common.TypeHelper;
import de.mknblch.nolisp.datatypes.ListStruct;
import de.mknblch.nolisp.datatypes.forms.BuiltInForm;

/**
 * @author mknblch
 */
public class OrForm extends BuiltInForm {

    @Override
    public String[] getSymbols() {
        return new String[]{"or"};
    }

    @Override
    public Object eval(ListStruct args) throws Exception {
        for (Object arg : args) {
            if (TypeHelper.asBoolean(arg)) return true;
        }
        return false;
    }
}    
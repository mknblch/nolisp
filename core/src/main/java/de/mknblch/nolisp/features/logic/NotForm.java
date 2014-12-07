package de.mknblch.nolisp.features.logic;

import de.mknblch.nolisp.common.TypeHelper;
import de.mknblch.nolisp.datatypes.ListStruct;
import de.mknblch.nolisp.datatypes.builtin.BuiltInForm;

/**
 * @author mknblch
 */
public class NotForm extends BuiltInForm {

    @Override
    public String[] getSymbols() {
        return new String[]{"not"};
    }

    @Override
    public Object eval(ListStruct args) throws Exception {
        return !TypeHelper.asBoolean(args.car());
    }
}    
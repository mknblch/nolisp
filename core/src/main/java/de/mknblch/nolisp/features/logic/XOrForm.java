package de.mknblch.nolisp.features.logic;

import de.mknblch.nolisp.common.TypeHelper;
import de.mknblch.nolisp.datatypes.ListStruct;
import de.mknblch.nolisp.datatypes.builtin.BuiltInForm;

/**
 * @author mknblch
 */
public class XOrForm extends BuiltInForm {

    @Override
    public String[] getSymbols() {
        return new String[]{"xor"};
    }

    @Override
    public Object eval(ListStruct args) throws Exception {
        return TypeHelper.asBoolean(args.car()) ^ TypeHelper.asBoolean(args.cadr());
    }
}    
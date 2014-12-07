package de.mknblch.nolisp.features.math;

import de.mknblch.nolisp.datatypes.ListStruct;
import de.mknblch.nolisp.datatypes.builtin.BuiltInForm;

import static de.mknblch.nolisp.common.TypeHelper.asInt;

/**
 * @author mknblch
 */
public class IntOrForm extends BuiltInForm {

    @Override
    public String[] getSymbols() {
        return new String[]{"int-or", "ior"};
    }

    @Override
    public Object eval(ListStruct args) throws Exception {
        int ret = 0;
        for (Object arg : args) {
            ret = ret | asInt(arg);
        }
        return ret;
    }
}    
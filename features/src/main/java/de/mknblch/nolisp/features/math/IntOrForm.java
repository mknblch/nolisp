package de.mknblch.nolisp.features.math;

import de.mknblch.nolisp.common.TypeHelper;
import de.mknblch.nolisp.datatypes.ListStruct;
import de.mknblch.nolisp.datatypes.builtin.BuiltInForm;
import de.mknblch.nolisp.generator.Define;

import static de.mknblch.nolisp.common.TypeHelper.asInt;

/**
 * @author mknblch
 */
@Define({"int-or", "ior"})
public class IntOrForm implements Form  {

    @Override
    public Object eval(ListStruct args) throws Exception {
        int ret = 0;
        for (Object arg : args) {
            ret = ret | TypeHelper.asInt(arg);
        }
        return ret;
    }
}    
package de.mknblch.nolisp.features.math;

import de.mknblch.nolisp.common.TypeHelper;
import de.mknblch.nolisp.datatypes.Form;
import de.mknblch.nolisp.datatypes.ListStruct;
import de.mknblch.nolisp.generator.Define;

/**
 * @author mknblch
 */
@Define({"int-and", "iand"})
public class IntAndForm implements Form  {

    @Override
    public Object eval(ListStruct args) throws Exception {
        int ret = 0xFFFFFFFF;
        for (Object arg : args) {
            ret = ret & TypeHelper.asInt(arg);
        }
        return ret;
    }
}    
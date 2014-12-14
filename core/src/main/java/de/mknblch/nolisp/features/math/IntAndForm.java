package de.mknblch.nolisp.features.math;

import de.mknblch.nolisp.datatypes.ListStruct;
import de.mknblch.nolisp.datatypes.builtin.BuiltInForm;
import de.mknblch.nolisp.scanner.Define;

import static de.mknblch.nolisp.common.TypeHelper.asInt;

/**
 * @author mknblch
 */
@Define({"int-and", "iand"})
public class IntAndForm extends BuiltInForm  {

    @Override
    public Object eval(ListStruct args) throws Exception {
        int ret = 0xFFFFFFFF;
        for (Object arg : args) {
            ret = ret & asInt(arg);
        }
        return ret;
    }
}    
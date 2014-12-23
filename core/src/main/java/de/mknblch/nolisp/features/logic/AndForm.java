package de.mknblch.nolisp.features.logic;

import de.mknblch.nolisp.common.TypeHelper;
import de.mknblch.nolisp.datatypes.ListStruct;
import de.mknblch.nolisp.dialect.builtin.BuiltInForm;
import de.mknblch.nolisp.codegen.Define;

/**
 * @author mknblch
 */
@Define({"and"})
public class AndForm extends BuiltInForm  {

    @Override
    public Object eval(ListStruct args) throws Exception {
        for (Object arg : args) {
            if (!TypeHelper.asBoolean(arg)) return false;
        }
        return true;
    }
}    
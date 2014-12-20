package de.mknblch.nolisp.features.logic;

import de.mknblch.nolisp.common.TypeHelper;
import de.mknblch.nolisp.datatypes.ListStruct;
import de.mknblch.nolisp.dialect.builtin.BuiltInForm;
import de.mknblch.nolisp.dialect.Define;

/**
 * @author mknblch
 */
@Define({"or"})
public class OrForm extends BuiltInForm  {

    @Override
    public Object eval(ListStruct args) throws Exception {
        for (Object arg : args) {
            if (TypeHelper.asBoolean(arg)) return true;
        }
        return false;
    }
}    
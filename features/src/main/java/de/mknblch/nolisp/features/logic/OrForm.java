package de.mknblch.nolisp.features.logic;

import de.mknblch.nolisp.common.TypeHelper;
import de.mknblch.nolisp.datatypes.Form;
import de.mknblch.nolisp.datatypes.ListStruct;
import de.mknblch.nolisp.generator.Define;

/**
 * @author mknblch
 */
@Define({"or"})
public class OrForm implements Form  {

    @Override
    public Object eval(ListStruct args) throws Exception {
        for (Object arg : args) {
            if (TypeHelper.asBoolean(arg)) return true;
        }
        return false;
    }
}    
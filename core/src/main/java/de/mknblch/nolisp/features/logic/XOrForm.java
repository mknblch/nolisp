package de.mknblch.nolisp.features.logic;

import de.mknblch.nolisp.common.TypeHelper;
import de.mknblch.nolisp.datatypes.ListStruct;
import de.mknblch.nolisp.dialect.builtin.BuiltInForm;
import de.mknblch.nolisp.dialect.Define;

/**
 * @author mknblch
 */
@Define({"xor"})
public class XOrForm extends BuiltInForm  {

    @Override
    public Object eval(ListStruct args) throws Exception {
        return TypeHelper.asBoolean(args.car()) ^ TypeHelper.asBoolean(args.cadr());
    }
}    
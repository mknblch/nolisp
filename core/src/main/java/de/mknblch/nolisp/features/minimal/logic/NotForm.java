package de.mknblch.nolisp.features.minimal.logic;

import de.mknblch.nolisp.common.TypeHelper;
import de.mknblch.nolisp.datatypes.ListStruct;
import de.mknblch.nolisp.datatypes.builtin.BuiltInForm;
import de.mknblch.nolisp.dialect.Define;

/**
 * @author mknblch
 */
@Define({"not"})
public class NotForm extends BuiltInForm  {

    @Override
    public Object eval(ListStruct args) throws Exception {
        return !TypeHelper.asBoolean(args.car());
    }
}    
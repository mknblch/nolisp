package de.mknblch.nolisp.features.java;

import de.mknblch.nolisp.common.TypeHelper;
import de.mknblch.nolisp.datatypes.ListStruct;
import de.mknblch.nolisp.datatypes.builtin.BuiltInForm;
import de.mknblch.nolisp.generator.Define;

import static de.mknblch.nolisp.common.TypeHelper.asException;

/**
 * @author mknblch
 */
@Define({"throw"})
public class ThrowForm extends BuiltInForm  {

    @Override
    public Object eval(ListStruct args) throws Exception {
        throw TypeHelper.asException(args.car());
    }
}    
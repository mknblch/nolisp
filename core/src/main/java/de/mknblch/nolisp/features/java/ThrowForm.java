package de.mknblch.nolisp.features.java;

import de.mknblch.nolisp.datatypes.ListStruct;
import de.mknblch.nolisp.dialect.builtin.BuiltInForm;
import de.mknblch.nolisp.codegen.Define;

import static de.mknblch.nolisp.common.TypeHelper.asException;

/**
 * @author mknblch
 */
@Define({"throw"})
public class ThrowForm extends BuiltInForm  {

    @Override
    public Object eval(ListStruct args) throws Exception {
        throw asException(args.car());
    }
}    
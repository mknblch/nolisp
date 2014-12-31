package de.mknblch.nolisp.features.java;

import de.mknblch.nolisp.common.TypeHelper;
import de.mknblch.nolisp.datatypes.Form;
import de.mknblch.nolisp.datatypes.ListStruct;
import de.mknblch.nolisp.generator.annotations.Define;

/**
 * @author mknblch
 */
@Define({"throw"})
public class ThrowForm implements Form  {

    @Override
    public Object eval(ListStruct args) throws Exception {
        throw TypeHelper.asException(args.car());
    }
}    
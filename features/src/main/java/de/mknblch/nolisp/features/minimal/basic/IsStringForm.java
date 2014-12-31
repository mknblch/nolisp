package de.mknblch.nolisp.features.minimal.basic;

import de.mknblch.nolisp.common.TypeHelper;
import de.mknblch.nolisp.datatypes.Form;
import de.mknblch.nolisp.datatypes.ListStruct;
import de.mknblch.nolisp.generator.annotations.Define;

/**
 * @author mknblch
 */
@Define({"string?"})
public class IsStringForm implements Form  {

    @Override
    public Object eval(ListStruct args) throws Exception {
        return TypeHelper.isString(args.car());
    }
}    
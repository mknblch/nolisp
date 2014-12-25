package de.mknblch.nolisp.features.number;

import de.mknblch.nolisp.common.TypeHelper;
import de.mknblch.nolisp.datatypes.Form;
import de.mknblch.nolisp.datatypes.ListStruct;
import de.mknblch.nolisp.generator.Define;

/**
 * @author mknblch
 */
@Define({"real?"})
public class IsRealForm implements Form  {

    @Override
    public Object eval(ListStruct args) throws Exception {
        return TypeHelper.isReal(args.car());
    }
}    
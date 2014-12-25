package de.mknblch.nolisp.features.math;

import de.mknblch.nolisp.common.TypeHelper;
import de.mknblch.nolisp.datatypes.Form;
import de.mknblch.nolisp.datatypes.ListStruct;
import de.mknblch.nolisp.generator.Define;

/**
 * @author mknblch
 */
@Define({"sqrt"})
public class SqrtForm implements Form  {

    @Override
    public Object eval(ListStruct args) throws Exception {
        return Math.sqrt(TypeHelper.asReal(args.car()));
    }
}    
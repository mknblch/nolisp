package de.mknblch.nolisp.features.minimal.math;

import de.mknblch.nolisp.common.TypeHelper;
import de.mknblch.nolisp.datatypes.Form;
import de.mknblch.nolisp.datatypes.ListStruct;
import de.mknblch.nolisp.generator.annotations.Define;

/**
 * @author mknblch
 */
@Define({"tan"})
public class TanForm implements Form  {

    @Override
    public Object eval(ListStruct args) throws Exception {
        return Math.tan(TypeHelper.asReal(args.car()));
    }
}    
package de.mknblch.nolisp.features.math;

import de.mknblch.nolisp.common.TypeHelper;
import de.mknblch.nolisp.datatypes.Form;
import de.mknblch.nolisp.datatypes.ListStruct;
import de.mknblch.nolisp.generator.Define;

/**
 * @author mknblch
 */
@Define({"<<"})
public class ShiftLeftForm implements Form  {

    @Override
    public Object eval(ListStruct args) throws Exception {
        return TypeHelper.asInt(args.car()) << TypeHelper.asInt(args.cadr()) ;
    }
}    
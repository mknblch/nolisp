package de.mknblch.nolisp.features.math;

import de.mknblch.nolisp.common.TypeHelper;
import de.mknblch.nolisp.datatypes.ListStruct;
import de.mknblch.nolisp.datatypes.builtin.BuiltInForm;
import de.mknblch.nolisp.generator.Define;

/**
 * @author mknblch
 */
@Define({"log10"})
public class Log10Form implements Form  {

    @Override
    public Object eval(ListStruct args) throws Exception {
        return Math.log10(TypeHelper.asReal(args.car()));
    }
}    
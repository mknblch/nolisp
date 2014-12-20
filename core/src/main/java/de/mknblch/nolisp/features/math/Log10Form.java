package de.mknblch.nolisp.features.math;

import de.mknblch.nolisp.common.TypeHelper;
import de.mknblch.nolisp.datatypes.ListStruct;
import de.mknblch.nolisp.dialect.builtin.BuiltInForm;
import de.mknblch.nolisp.dialect.Define;

/**
 * @author mknblch
 */
@Define({"log10"})
public class Log10Form extends BuiltInForm  {

    @Override
    public Object eval(ListStruct args) throws Exception {
        return Math.log10(TypeHelper.asReal(args.car()));
    }
}    
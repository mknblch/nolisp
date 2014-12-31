package de.mknblch.nolisp.features.minimal.math;

import de.mknblch.nolisp.common.Expectations;
import de.mknblch.nolisp.common.TypeHelper;
import de.mknblch.nolisp.datatypes.Form;
import de.mknblch.nolisp.datatypes.ListStruct;
import de.mknblch.nolisp.generator.annotations.Define;

/**
 * @author mknblch
 */
@Define({"**", "pow"})
public class PowForm implements Form  {

    @Override
    public Object eval(ListStruct args) throws Exception {
        Expectations.expectCdr(args);
        return Math.pow(
                TypeHelper.asReal(args.car()),
                TypeHelper.asReal(args.cdr().car()));
    }
}    
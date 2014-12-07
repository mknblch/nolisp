package de.mknblch.nolisp.features.math;

import de.mknblch.nolisp.common.Expectations;
import de.mknblch.nolisp.common.TypeHelper;
import de.mknblch.nolisp.datatypes.ListStruct;
import de.mknblch.nolisp.datatypes.builtin.BuiltInForm;

/**
 * @author mknblch
 */
public class PowForm extends BuiltInForm {

    @Override
    public String[] getSymbols() {
        return new String[]{"**", "pow"};
    }

    @Override
    public Object eval(ListStruct args) throws Exception {
        Expectations.expectCdr(args);
        return Math.pow(
                TypeHelper.asReal(args.car()),
                TypeHelper.asReal(args.cdr().car()));
    }
}    
package de.mknblch.nolisp.features.predicate;

import de.mknblch.nolisp.common.TypeHelper;
import de.mknblch.nolisp.datatypes.ListStruct;
import de.mknblch.nolisp.datatypes.forms.BuiltInForm;

/**
 * @author mknblch
 */
public class IsRealForm extends BuiltInForm {

    @Override
    public String[] getSymbols() {
        return new String[]{"real?"};
    }

    @Override
    public Object eval(ListStruct args) throws Exception {
        return TypeHelper.isReal(args.car());
    }
}    
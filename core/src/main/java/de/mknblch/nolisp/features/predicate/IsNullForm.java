package de.mknblch.nolisp.features.predicate;

import de.mknblch.nolisp.datatypes.ListStruct;
import de.mknblch.nolisp.datatypes.forms.BuiltInForm;

/**
 * @author mknblch
 */
public class IsNullForm extends BuiltInForm {

    @Override
    public String[] getSymbols() {
        return new String[]{"null?"};
    }

    @Override
    public Object eval(ListStruct args) throws Exception {
        return null == args.car();
    }
}    
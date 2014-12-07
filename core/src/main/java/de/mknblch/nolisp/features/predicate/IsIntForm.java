package de.mknblch.nolisp.features.predicate;

import de.mknblch.nolisp.datatypes.ListStruct;
import de.mknblch.nolisp.datatypes.builtin.BuiltInForm;

/**
 * @author mknblch
 */
public class IsIntForm extends BuiltInForm {

    @Override
    public String[] getSymbols() {
        return new String[]{"int?"};
    }

    @Override
    public Object eval(ListStruct args) throws Exception {
        return args.car() instanceof Integer;
    }
}    
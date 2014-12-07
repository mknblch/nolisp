package de.mknblch.nolisp.features.predicate;

import de.mknblch.nolisp.common.TypeHelper;
import de.mknblch.nolisp.datatypes.ListStruct;
import de.mknblch.nolisp.datatypes.forms.BuiltInForm;

/**
 * @author mknblch
 */
public class IsListForm extends BuiltInForm {

    @Override
    public String[] getSymbols() {
        return new String[]{"list?"};
    }

    @Override
    public Object eval(ListStruct args) throws Exception {
        return TypeHelper.isList(args.car());
    }
}    
package de.mknblch.nolisp.features.list;

import de.mknblch.nolisp.datatypes.ListStruct;
import de.mknblch.nolisp.datatypes.forms.BuiltInForm;

import static de.mknblch.nolisp.common.TypeHelper.asList;

/**
 * @author mknblch
 */
public class ListLengthForm extends BuiltInForm {

    @Override
    public String[] getSymbols() {
        return new String[]{"list-length", "llength"};
    }

    @Override
    public Object eval(ListStruct args) throws Exception {
        if (null == args) return 0;
        return asList(args.car()).size();
    }
}    
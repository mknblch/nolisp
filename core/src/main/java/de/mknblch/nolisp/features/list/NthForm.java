package de.mknblch.nolisp.features.list;

import de.mknblch.nolisp.datatypes.ListStruct;
import de.mknblch.nolisp.datatypes.forms.BuiltInForm;

import static de.mknblch.nolisp.common.TypeHelper.asInt;
import static de.mknblch.nolisp.common.TypeHelper.asList;

/**
 * @author mknblch
 */
public class NthForm extends BuiltInForm {

    @Override
    public String[] getSymbols() {
        return new String[]{"nth"};
    }

    @Override
    public Object eval(ListStruct args) throws Exception {
        return asList(args.cadr()).nth(asInt(args.car()));
    }
}    
package de.mknblch.nolisp.features.lambda;

import de.mknblch.nolisp.common.TypeHelper;
import de.mknblch.nolisp.datatypes.ListStruct;
import de.mknblch.nolisp.datatypes.forms.BuiltInForm;

/**
 * @author mknblch
 */
public class LBodyForm extends BuiltInForm {

    @Override
    public String[] getSymbols() {
        return new String[]{"lbody"};
    }

    @Override
    public Object eval(ListStruct args) throws Exception {
        return TypeHelper.asLambda(args.car()).getForm();
    }
}    
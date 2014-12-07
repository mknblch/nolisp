package de.mknblch.nolisp.features.lambda;

import de.mknblch.nolisp.common.TypeHelper;
import de.mknblch.nolisp.datatypes.ListStruct;
import de.mknblch.nolisp.datatypes.builtin.BuiltInForm;

/**
 * @author mknblch
 */
public class LArgsForm extends BuiltInForm {

    @Override
    public String[] getSymbols() {
        return new String[]{"largs"};
    }

    @Override
    public Object eval(ListStruct args) throws Exception {
        return TypeHelper.asLambda(args.car()).getArgumentSymbols();
    }
}    
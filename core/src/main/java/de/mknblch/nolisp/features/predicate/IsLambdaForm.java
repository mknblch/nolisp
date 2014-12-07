package de.mknblch.nolisp.features.predicate;

import de.mknblch.nolisp.common.TypeHelper;
import de.mknblch.nolisp.datatypes.ListStruct;
import de.mknblch.nolisp.datatypes.builtin.BuiltInForm;

/**
 * @author mknblch
 */
public class IsLambdaForm extends BuiltInForm {

    @Override
    public String[] getSymbols() {
        return new String[]{"lambda?"};
    }

    @Override
    public Object eval(ListStruct args) throws Exception {
        return TypeHelper.isLambda(args.car());
    }
}    
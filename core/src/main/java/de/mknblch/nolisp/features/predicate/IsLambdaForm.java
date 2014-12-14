package de.mknblch.nolisp.features.predicate;

import de.mknblch.nolisp.common.TypeHelper;
import de.mknblch.nolisp.datatypes.ListStruct;
import de.mknblch.nolisp.datatypes.builtin.BuiltInForm;
import de.mknblch.nolisp.scanner.Define;

/**
 * @author mknblch
 */
@Define({"lambda?"})
public class IsLambdaForm extends BuiltInForm  {

    @Override
    public Object eval(ListStruct args) throws Exception {
        return TypeHelper.isLambda(args.car());
    }
}    
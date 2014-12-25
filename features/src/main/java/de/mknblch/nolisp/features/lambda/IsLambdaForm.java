package de.mknblch.nolisp.features.lambda;

import de.mknblch.nolisp.datatypes.Form;
import de.mknblch.nolisp.datatypes.ListStruct;
import de.mknblch.nolisp.generator.Define;

/**
 * @author mknblch
 */
@Define({"lambda?"})
public class IsLambdaForm implements Form  {

    @Override
    public Object eval(ListStruct args) throws Exception {
        return args.car() instanceof Lambda;
    }
}    
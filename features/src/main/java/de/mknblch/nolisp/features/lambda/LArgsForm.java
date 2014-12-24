package de.mknblch.nolisp.features.lambda;

import de.mknblch.nolisp.common.TypeHelper;
import de.mknblch.nolisp.datatypes.ListStruct;
import de.mknblch.nolisp.datatypes.builtin.BuiltInForm;
import de.mknblch.nolisp.generator.Define;

/**
 * @author mknblch
 */
@Define({"largs"})
public class LArgsForm implements Form  {

    @Override
    public Object eval(ListStruct args) throws Exception {
        return Lambda.asLambda(args.car()).getArgumentSymbols();
    }
}    
package de.mknblch.nolisp.features.lambda;

import de.mknblch.nolisp.datatypes.Form;
import de.mknblch.nolisp.datatypes.ListStruct;
import de.mknblch.nolisp.generator.annotations.Define;

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
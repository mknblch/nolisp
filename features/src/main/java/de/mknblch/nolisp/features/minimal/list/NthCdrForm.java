package de.mknblch.nolisp.features.minimal.list;

import de.mknblch.nolisp.common.TypeHelper;
import de.mknblch.nolisp.datatypes.Form;
import de.mknblch.nolisp.datatypes.ListStruct;
import de.mknblch.nolisp.generator.annotations.Define;

/**
 * @author mknblch
 */
@Define({"nthcdr"})
public class NthCdrForm implements Form  {

    @Override
    public Object eval(ListStruct args) throws Exception {
        return TypeHelper.asList(args.cadr()).nthcdr(TypeHelper.asInt(args.car()));
    }
}    
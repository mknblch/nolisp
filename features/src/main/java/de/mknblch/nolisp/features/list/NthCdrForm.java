package de.mknblch.nolisp.features.list;

import de.mknblch.nolisp.common.TypeHelper;
import de.mknblch.nolisp.datatypes.ListStruct;
import de.mknblch.nolisp.datatypes.builtin.BuiltInForm;
import de.mknblch.nolisp.generator.Define;

import static de.mknblch.nolisp.common.TypeHelper.asInt;
import static de.mknblch.nolisp.common.TypeHelper.asList;

/**
 * @author mknblch
 */
@Define({"nthcdr"})
public class NthCdrForm extends BuiltInForm  {

    @Override
    public Object eval(ListStruct args) throws Exception {
        return TypeHelper.asList(args.cadr()).nthcdr(TypeHelper.asInt(args.car()));
    }
}    
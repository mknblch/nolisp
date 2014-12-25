package de.mknblch.nolisp.features.array;

import de.mknblch.nolisp.common.TypeHelper;
import de.mknblch.nolisp.datatypes.Form;
import de.mknblch.nolisp.datatypes.ListStruct;
import de.mknblch.nolisp.generator.Define;

/**
 * @author mknblch
 */
@Define({"array-init", "ainit"})
public class ArrayInitForm implements Form {

    @Override
    public Object eval(ListStruct args) throws Exception {
        return new Object[TypeHelper.asInt(args.car())];
    }
}

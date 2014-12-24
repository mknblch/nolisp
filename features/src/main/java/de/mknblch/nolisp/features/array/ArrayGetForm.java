package de.mknblch.nolisp.features.array;

import de.mknblch.nolisp.common.TypeHelper;
import de.mknblch.nolisp.datatypes.Form;
import de.mknblch.nolisp.datatypes.ListStruct;
import de.mknblch.nolisp.datatypes.builtin.BuiltInForm;
import de.mknblch.nolisp.generator.Define;

import static de.mknblch.nolisp.common.TypeHelper.asArray;
import static de.mknblch.nolisp.common.TypeHelper.asInt;

/**
 * @author mknblch
 */
@Define({"array-get", "aget"})
public class ArrayGetForm implements Form {

    @Override
    public Object eval(ListStruct args) throws Exception {
        return TypeHelper.asArray(args.car()) [ TypeHelper.asInt(args.cadr()) ];
    }
}

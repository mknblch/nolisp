package de.mknblch.nolisp.features.array;

import de.mknblch.nolisp.datatypes.ListStruct;
import de.mknblch.nolisp.dialect.builtin.BuiltInForm;
import de.mknblch.nolisp.dialect.Define;

import static de.mknblch.nolisp.common.TypeHelper.asArray;
import static de.mknblch.nolisp.common.TypeHelper.asInt;

/**
 * @author mknblch
 */
@Define({"array-get", "aget"})
public class ArrayGetForm extends BuiltInForm {

    @Override
    public Object eval(ListStruct args) throws Exception {
        return asArray(args.car()) [ asInt(args.cadr()) ];
    }
}

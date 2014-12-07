package de.mknblch.nolisp.features.array;

import de.mknblch.nolisp.datatypes.ListStruct;
import de.mknblch.nolisp.datatypes.forms.BuiltInForm;

import static de.mknblch.nolisp.common.TypeHelper.asArray;
import static de.mknblch.nolisp.common.TypeHelper.asInt;

/**
 * @author mknblch
 */
public class ArrayGetForm extends BuiltInForm{
    @Override
    public String[] getSymbols() {
        return new String[]{"array-get", "aget"};
    }

    @Override
    public Object eval(ListStruct args) throws Exception {
        return asArray(args.car()) [ asInt(args.cadr()) ];
    }
}

package de.mknblch.nolisp.features.array;

import de.mknblch.nolisp.datatypes.ListStruct;
import de.mknblch.nolisp.datatypes.forms.BuiltInForm;
import de.mknblch.nolisp.datatypes.forms.Form;

import static de.mknblch.nolisp.common.TypeHelper.asArray;
import static de.mknblch.nolisp.common.TypeHelper.asInt;

/**
 * @author mknblch
 */
public class ArrayGet extends BuiltInForm{
    @Override
    public String getSymbol() {
        return "array-get";
    }

    @Override
    public Object eval(ListStruct args) throws Exception {
        return asArray(args.car()) [ asInt(args.cadr()) ];
    }
}

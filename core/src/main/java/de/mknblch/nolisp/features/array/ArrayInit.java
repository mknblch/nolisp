package de.mknblch.nolisp.features.array;

import de.mknblch.nolisp.common.Expectations;
import de.mknblch.nolisp.datatypes.ListStruct;
import de.mknblch.nolisp.datatypes.forms.BuiltInForm;

import static de.mknblch.nolisp.common.TypeHelper.asArray;
import static de.mknblch.nolisp.common.TypeHelper.asInt;

/**
 * @author mknblch
 */
public class ArrayInit extends BuiltInForm{
    @Override
    public String getSymbol() {
        return "array-init";
    }

    @Override
    public Object eval(ListStruct args) throws Exception {
        return new Object[asInt(args.car())];
    }
}

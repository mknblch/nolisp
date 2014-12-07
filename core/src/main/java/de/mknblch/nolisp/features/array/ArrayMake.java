package de.mknblch.nolisp.features.array;

import de.mknblch.nolisp.common.Expectations;
import de.mknblch.nolisp.datatypes.ListStruct;
import de.mknblch.nolisp.datatypes.forms.BuiltInForm;

import java.util.ArrayList;

import static de.mknblch.nolisp.common.TypeHelper.asArray;
import static de.mknblch.nolisp.common.TypeHelper.asInt;

/**
 * @author mknblch
 */
public class ArrayMake extends BuiltInForm{
    @Override
    public String getSymbol() {
        return "array-make";
    }

    @Override
    public Object eval(ListStruct args) throws Exception {
        if (null == args) {
            return new Object[0];
        }
        final ArrayList<Object> objects = new ArrayList<>();
        for (Object arg : args) {
            objects.add(arg);
        }
        return objects.toArray();
    }
}

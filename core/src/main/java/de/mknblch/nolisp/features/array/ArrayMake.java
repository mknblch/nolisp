package de.mknblch.nolisp.features.array;

import de.mknblch.nolisp.datatypes.ListStruct;
import de.mknblch.nolisp.datatypes.forms.BuiltInForm;

import java.util.ArrayList;

/**
 * @author mknblch
 */
public class ArrayMake extends BuiltInForm{
    @Override
    public String[] getSymbols() {
        return new String[]{"array-make"};
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

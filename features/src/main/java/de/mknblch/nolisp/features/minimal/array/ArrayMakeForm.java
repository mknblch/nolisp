package de.mknblch.nolisp.features.minimal.array;

import de.mknblch.nolisp.datatypes.Form;
import de.mknblch.nolisp.datatypes.ListStruct;
import de.mknblch.nolisp.generator.annotations.Define;

import java.util.ArrayList;

/**
 * @author mknblch
 */
@Define({"array-make", "amake"})
public class ArrayMakeForm implements Form {

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

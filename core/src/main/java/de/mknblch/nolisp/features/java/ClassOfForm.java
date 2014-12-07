package de.mknblch.nolisp.features.java;

import de.mknblch.nolisp.datatypes.ListStruct;
import de.mknblch.nolisp.datatypes.builtin.BuiltInForm;

/**
 * @author mknblch
 */
public class ClassOfForm extends BuiltInForm {

    @Override
    public String[] getSymbols() {
        return new String[]{"classof"};
    }

    @Override
    public Object eval(ListStruct args) throws Exception {
        final Object car = args.car();
        if (null == car) {
            return null;
        }
        return car.getClass();
    }
}    
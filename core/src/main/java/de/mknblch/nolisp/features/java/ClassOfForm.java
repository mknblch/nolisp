package de.mknblch.nolisp.features.java;

import de.mknblch.nolisp.datatypes.ListStruct;
import de.mknblch.nolisp.datatypes.builtin.BuiltInForm;
import de.mknblch.nolisp.scanner.Define;

/**
 * @author mknblch
 */
@Define({"classof"})
public class ClassOfForm extends BuiltInForm  {

    @Override
    public Object eval(ListStruct args) throws Exception {
        final Object car = args.car();
        if (null == car) {
            return null;
        }
        return car.getClass();
    }
}    
package de.mknblch.nolisp.features.java;

import de.mknblch.nolisp.datatypes.ListStruct;
import de.mknblch.nolisp.dialect.builtin.BuiltInForm;
import de.mknblch.nolisp.codegen.Define;

/**
 * @author mknblch
 */
@Define({"classof", "classOf"})
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
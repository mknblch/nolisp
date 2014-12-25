package de.mknblch.nolisp.features.java;

import de.mknblch.nolisp.datatypes.Form;
import de.mknblch.nolisp.datatypes.ListStruct;
import de.mknblch.nolisp.generator.Define;

/**
 * @author mknblch
 */
@Define({"classof", "classOf"})
public class ClassOfForm implements Form  {

    @Override
    public Object eval(ListStruct args) throws Exception {
        final Object car = args.car();
        if (null == car) {
            return null;
        }
        return car.getClass();
    }
}    
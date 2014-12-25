package de.mknblch.nolisp.features.list;

import de.mknblch.nolisp.common.TypeHelper;
import de.mknblch.nolisp.datatypes.Form;
import de.mknblch.nolisp.datatypes.ListStruct;
import de.mknblch.nolisp.generator.Define;

/**
 * @author mknblch
 */
@Define({"car"})
public class CarForm implements Form  {

    @Override
    public Object eval(ListStruct args) throws Exception {
        return TypeHelper.asList(args.car()).car();
    }
}    
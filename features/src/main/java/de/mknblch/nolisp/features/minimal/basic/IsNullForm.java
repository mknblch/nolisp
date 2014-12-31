package de.mknblch.nolisp.features.minimal.basic;

import de.mknblch.nolisp.datatypes.Form;
import de.mknblch.nolisp.datatypes.ListStruct;
import de.mknblch.nolisp.generator.annotations.Define;

/**
 * @author mknblch
 */
@Define({"null?"})
public class IsNullForm implements Form  {

    @Override
    public Object eval(ListStruct args) throws Exception {
        return null == args.car();
    }
}    
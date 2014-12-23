package de.mknblch.nolisp.features.minimal;

import de.mknblch.nolisp.datatypes.ListStruct;
import de.mknblch.nolisp.datatypes.builtin.BuiltInForm;
import de.mknblch.nolisp.generator.Define;

/**
 * @author mknblch
 */
@Define({"null?"})
public class IsNullForm extends BuiltInForm  {

    @Override
    public Object eval(ListStruct args) throws Exception {
        return null == args.car();
    }
}    
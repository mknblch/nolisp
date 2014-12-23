package de.mknblch.nolisp.features.list;

import de.mknblch.nolisp.datatypes.ListStruct;
import de.mknblch.nolisp.datatypes.builtin.BuiltInForm;
import de.mknblch.nolisp.generator.Define;

/**
 * @author mknblch
 */
@Define({"list"})
public class ListForm extends BuiltInForm  {

    @Override
    public Object eval(ListStruct args) throws Exception {
        return args;
    }
}    
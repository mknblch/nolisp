package de.mknblch.nolisp.features.list;

import de.mknblch.nolisp.datatypes.Form;
import de.mknblch.nolisp.datatypes.ListStruct;
import de.mknblch.nolisp.generator.Define;

/**
 * @author mknblch
 */
@Define({"list"})
public class ListForm implements Form  {

    @Override
    public Object eval(ListStruct args) throws Exception {
        return args;
    }
}    
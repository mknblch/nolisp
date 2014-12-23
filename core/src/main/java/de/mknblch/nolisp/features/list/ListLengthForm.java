package de.mknblch.nolisp.features.list;

import de.mknblch.nolisp.datatypes.ListStruct;
import de.mknblch.nolisp.datatypes.builtin.BuiltInForm;
import de.mknblch.nolisp.generator.Define;

import static de.mknblch.nolisp.common.TypeHelper.asList;

/**
 * @author mknblch
 */
@Define({"list-length", "llength"})
public class ListLengthForm extends BuiltInForm  {

    @Override
    public Object eval(ListStruct args) throws Exception {
        if (null == args) return 0;
        return asList(args.car()).size();
    }
}    
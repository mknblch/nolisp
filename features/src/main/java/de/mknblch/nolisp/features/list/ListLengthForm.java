package de.mknblch.nolisp.features.list;

import de.mknblch.nolisp.common.TypeHelper;
import de.mknblch.nolisp.datatypes.Form;
import de.mknblch.nolisp.datatypes.ListStruct;
import de.mknblch.nolisp.generator.Define;

/**
 * @author mknblch
 */
@Define({"list-length", "llength"})
public class ListLengthForm implements Form  {

    @Override
    public Object eval(ListStruct args) throws Exception {
        if (null == args) return 0;
        return TypeHelper.asList(args.car()).size();
    }
}    
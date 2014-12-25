package de.mknblch.nolisp.features.list;

import de.mknblch.nolisp.common.TypeHelper;
import de.mknblch.nolisp.datatypes.Form;
import de.mknblch.nolisp.datatypes.ListStruct;
import de.mknblch.nolisp.generator.Define;

/**
 * @author mknblch
 */
@Define({"list?"})
public class IsListForm implements Form  {

    @Override
    public Object eval(ListStruct args) throws Exception {
        return TypeHelper.isList(args.car());
    }
}    
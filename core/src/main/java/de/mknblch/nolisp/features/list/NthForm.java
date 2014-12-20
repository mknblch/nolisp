package de.mknblch.nolisp.features.list;

import de.mknblch.nolisp.datatypes.ListStruct;
import de.mknblch.nolisp.dialect.builtin.BuiltInForm;
import de.mknblch.nolisp.dialect.Define;

import static de.mknblch.nolisp.common.TypeHelper.asInt;
import static de.mknblch.nolisp.common.TypeHelper.asList;

/**
 * @author mknblch
 */
@Define({"nth"})
public class NthForm extends BuiltInForm  {

    @Override
    public Object eval(ListStruct args) throws Exception {
        return asList(args.cadr()).nth(asInt(args.car()));
    }
}    
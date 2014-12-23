package de.mknblch.nolisp.features.list;

import de.mknblch.nolisp.datatypes.ListStruct;
import de.mknblch.nolisp.datatypes.builtin.BuiltInForm;
import de.mknblch.nolisp.generator.Define;

import static de.mknblch.nolisp.common.TypeHelper.asList;

/**
 * @author mknblch
 */
@Define({"cdr"})
public class CdrForm extends BuiltInForm  {

    @Override
    public Object eval(ListStruct args) throws Exception {
        return asList(args.car()).cdr();
    }
}    
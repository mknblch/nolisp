package de.mknblch.nolisp.features.minimal;

import de.mknblch.nolisp.datatypes.ListStruct;
import de.mknblch.nolisp.datatypes.builtin.BuiltInForm;
import de.mknblch.nolisp.dialect.Define;

/**
 * @author mknblch
 */
@Define({"progn"})
public class PrognForm extends BuiltInForm  {

    @Override
    public Object eval(ListStruct args) throws Exception {
        if (null == args) {
            return null;
        }
        return args.last().car();
    }
}    
package de.mknblch.nolisp.features.minimal;

import de.mknblch.nolisp.common.TypeHelper;
import de.mknblch.nolisp.datatypes.ListStruct;
import de.mknblch.nolisp.dialect.builtin.BuiltInForm;
import de.mknblch.nolisp.dialect.Define;

/**
 * @author mknblch
 */
@Define({"atom?"})
public class IsAtomForm extends BuiltInForm  {

    @Override
    public Object eval(ListStruct args) throws Exception {
        return TypeHelper.isAtom(args.car());
    }
}    
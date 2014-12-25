package de.mknblch.nolisp.features.minimal;

import de.mknblch.nolisp.common.TypeHelper;
import de.mknblch.nolisp.datatypes.Form;
import de.mknblch.nolisp.datatypes.ListStruct;
import de.mknblch.nolisp.generator.Define;

/**
 * @author mknblch
 */
@Define({"atom?"})
public class IsAtomForm implements Form  {

    @Override
    public Object eval(ListStruct args) throws Exception {
        return TypeHelper.isAtom(args.car());
    }
}    
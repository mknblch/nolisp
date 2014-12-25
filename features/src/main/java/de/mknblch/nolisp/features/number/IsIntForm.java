package de.mknblch.nolisp.features.number;

import de.mknblch.nolisp.datatypes.Form;
import de.mknblch.nolisp.datatypes.ListStruct;
import de.mknblch.nolisp.generator.Define;

/**
 * @author mknblch
 */
@Define({"int?"})
public class IsIntForm implements Form  {

    @Override
    public Object eval(ListStruct args) throws Exception {
        return args.car() instanceof Integer;
    }
}    
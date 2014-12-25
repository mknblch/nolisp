package de.mknblch.nolisp.features.minimal;

import de.mknblch.nolisp.datatypes.Form;
import de.mknblch.nolisp.datatypes.ListStruct;
import de.mknblch.nolisp.generator.Define;

/**
 * @author mknblch
 */
@Define({"progn"})
public class PrognForm implements Form  {

    @Override
    public Object eval(ListStruct args) throws Exception {
        if (null == args) {
            return null;
        }
        return args.last().car();
    }
}    
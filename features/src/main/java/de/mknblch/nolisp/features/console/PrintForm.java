package de.mknblch.nolisp.features.console;

import de.mknblch.nolisp.datatypes.ListStruct;
import de.mknblch.nolisp.datatypes.builtin.BuiltInForm;
import de.mknblch.nolisp.generator.Define;

/**
 * @author mknblch
 */
@Define({"print"})
public class PrintForm implements Form  {

    @Override
    public Object eval(ListStruct args) throws Exception {
        System.out.println(args.car());
        return args.car();
    }
}    
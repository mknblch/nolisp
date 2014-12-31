package de.mknblch.nolisp.features.console;

import de.mknblch.nolisp.datatypes.Form;
import de.mknblch.nolisp.datatypes.ListStruct;
import de.mknblch.nolisp.generator.annotations.Define;

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
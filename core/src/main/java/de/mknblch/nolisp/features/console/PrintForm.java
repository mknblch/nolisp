package de.mknblch.nolisp.features.console;

import de.mknblch.nolisp.datatypes.ListStruct;
import de.mknblch.nolisp.datatypes.builtin.BuiltInForm;
import de.mknblch.nolisp.scanner.Define;

/**
 * @author mknblch
 */
@Define({"print"})
public class PrintForm extends BuiltInForm  {

    @Override
    public Object eval(ListStruct args) throws Exception {
        System.out.println(args.car());
        return args.car();
    }
}    
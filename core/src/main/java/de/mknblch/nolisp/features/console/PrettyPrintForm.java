package de.mknblch.nolisp.features.console;

import de.mknblch.nolisp.common.FormatHelper;
import de.mknblch.nolisp.datatypes.ListStruct;
import de.mknblch.nolisp.datatypes.builtin.BuiltInForm;
import de.mknblch.nolisp.codegen.Define;

/**
 * @author mknblch
 */
@Define({"pprint"})
public class PrettyPrintForm extends BuiltInForm  {

    @Override
    public Object eval(ListStruct args) throws Exception {
        System.out.println(FormatHelper.formatPretty(args.car()));
        return args.car();
    }
}    
package de.mknblch.nolisp.features.console;

import de.mknblch.nolisp.common.FormatHelper;
import de.mknblch.nolisp.datatypes.Form;
import de.mknblch.nolisp.datatypes.ListStruct;
import de.mknblch.nolisp.generator.annotations.Define;

/**
 * @author mknblch
 */
@Define({"pprint"})
public class PrettyPrintForm implements Form  {

    @Override
    public Object eval(ListStruct args) throws Exception {
        System.out.println(FormatHelper.formatPretty(args.car()));
        return args.car();
    }
}    
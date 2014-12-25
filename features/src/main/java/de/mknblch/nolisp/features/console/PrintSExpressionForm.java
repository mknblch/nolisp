package de.mknblch.nolisp.features.console;

import de.mknblch.nolisp.common.FormatHelper;
import de.mknblch.nolisp.datatypes.Form;
import de.mknblch.nolisp.datatypes.ListStruct;
import de.mknblch.nolisp.generator.Define;

/**
 * @author mknblch
 */
@Define({"sprint"})
public class PrintSExpressionForm implements Form  {

    @Override
    public Object eval(ListStruct args) throws Exception {
        System.out.println(FormatHelper.formatAsSExpression(args));
        return args.car();
    }
}    
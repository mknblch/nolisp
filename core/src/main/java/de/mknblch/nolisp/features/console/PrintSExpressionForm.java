package de.mknblch.nolisp.features.console;

import de.mknblch.nolisp.common.FormatHelper;
import de.mknblch.nolisp.datatypes.ListStruct;
import de.mknblch.nolisp.dialect.builtin.BuiltInForm;
import de.mknblch.nolisp.dialect.Define;

/**
 * @author mknblch
 */
@Define({"sprint"})
public class PrintSExpressionForm extends BuiltInForm  {

    @Override
    public Object eval(ListStruct args) throws Exception {
        System.out.println(FormatHelper.formatAsSExpression(args));
        return args.car();
    }
}    
package de.mknblch.nolisp.features.console;

import de.mknblch.nolisp.common.FormatHelper;
import de.mknblch.nolisp.datatypes.ListStruct;
import de.mknblch.nolisp.datatypes.forms.BuiltInForm;

/**
 * @author mknblch
 */
public class PrintSExpressionForm extends BuiltInForm {

    @Override
    public String[] getSymbols() {
        return new String[]{"sprint"};
    }

    @Override
    public Object eval(ListStruct args) throws Exception {
        System.out.println(FormatHelper.formatAsSExpression(args));
        return args.car();
    }
}    
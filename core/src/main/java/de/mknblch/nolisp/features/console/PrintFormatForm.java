package de.mknblch.nolisp.features.console;

import de.mknblch.nolisp.common.TypeHelper;
import de.mknblch.nolisp.datatypes.ListStruct;
import de.mknblch.nolisp.datatypes.builtin.BuiltInForm;

/**
 * @author mknblch
 */
public class PrintFormatForm extends BuiltInForm {

    @Override
    public String[] getSymbols() {
        return new String[]{"printf"};
    }

    @Override
    public Object eval(ListStruct args) throws Exception {
        final String format = TypeHelper.asString(args.car());
        final int size = args.size() - 1;
        final Object[] objects = new Object[size];
        int count = 0;
        for (Object o : args.cdr()) {
            objects[count++] = o;
        }
        System.out.printf(format, objects);
        return args.car();
    }
}    
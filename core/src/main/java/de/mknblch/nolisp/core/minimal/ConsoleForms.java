package de.mknblch.nolisp.core.minimal;

import de.mknblch.nolisp.core.common.FormatHelper;
import de.mknblch.nolisp.core.common.TypeHelper;
import de.mknblch.nolisp.core.interpreter.Context;
import de.mknblch.nolisp.core.interpreter.structs.ListStruct;
import de.mknblch.nolisp.core.scanner.Define;

/**
 * @author mknblch
 */
public class ConsoleForms {

    @Define("print")
    public static Object print(Context context, ListStruct args) throws Exception {
        System.out.print(args.car());
        return null;
    }

    @Define("pprint")
    public static Object pprint(Context context, ListStruct args) throws Exception {
        System.out.print(FormatHelper.formatPretty(args.car()));
        return null;
    }

    @Define("sprint")
    public static Object sprint(Context context, ListStruct args) throws Exception {
        System.out.print(FormatHelper.formatAsSExpression(args));
        return null;
    }

    @Define("printf")
    public static Object printf(Context context, ListStruct args) throws Exception {
        final String format = TypeHelper.asString(args.car());
        final int size = args.size() - 1;
        final Object[] objects = new Object[size];
        int count = 0;
        for (Object o : args.cdr()) {
            objects[count++] = o;
        }
        System.out.printf(format, objects);
        return null;
    }


}

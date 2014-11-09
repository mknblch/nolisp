package de.mknblch.nolisp.minilisp.forms;

import de.mknblch.nolisp.annotations.Define;
import de.mknblch.nolisp.ast.ListStruct;
import de.mknblch.nolisp.helper.TypeHelper;
import de.mknblch.nolisp.interpreter.Context;

/**
 * @author mknblch
 */
public class ConsoleForms {

    @Define("print")
    public static Object print(Context context, ListStruct args) throws Exception {
        System.out.print(args.car());
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

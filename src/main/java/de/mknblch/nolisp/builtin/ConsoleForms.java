package de.mknblch.nolisp.builtin;

import de.mknblch.nolisp.interpreter.Context;
import de.mknblch.nolisp.func.Define;
import de.mknblch.nolisp.helper.TypeHelper;
import de.mknblch.nolisp.ast.ListStruct;

/**
 * @author mknblch
 */
public class ConsoleForms {

    @Define(value = "print")
    public static Object print(Context context, ListStruct args) throws Exception {
        System.out.print(args.car());
        return null;
    }

    @Define(value = "printf")
    public static Object printf(Context context, ListStruct args) throws Exception {
        final String format = TypeHelper.asString(args.car());
        final int size = args.size()-1;
        final Object[] objects = new Object[size];
        int count = 0;
        for (Object o : args.cdr()) {
            objects[count++] = o;
        }
        System.out.printf(format, objects);
        return null;
    }
}

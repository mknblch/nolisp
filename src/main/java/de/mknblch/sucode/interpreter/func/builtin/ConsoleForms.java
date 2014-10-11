package de.mknblch.sucode.interpreter.func.builtin;

import de.mknblch.sucode.interpreter.Context;
import de.mknblch.sucode.interpreter.Interpreter;
import de.mknblch.sucode.interpreter.func.Define;
import de.mknblch.sucode.interpreter.func.TypeHelper;
import de.mknblch.sucode.parser.structs.ListStruct;

/**
 * Created by mknblch on 12.10.2014.
 */
public class ConsoleForms {

    @Define(symbol = "print")
    public static Object print(ListStruct args, Context context) throws Exception {
        System.out.print(Interpreter.eval(args.car(), context));
        return null;
    }

    @Define(symbol = "printf")
    public static Object printf(ListStruct args, Context context) throws Exception {
        final String format = TypeHelper.asString(Interpreter.eval(args.car(), context));
        final int size = args.size()-1;
        final Object[] objects = new Object[size];
        int count = 0;
        for (Object o : args.cdr()) {
            objects[count++] = Interpreter.eval(o, context);
        }
        System.out.printf(format, objects);
        return null;
    }
}

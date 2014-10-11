package de.mknblch.sucode.interpreter.forms.builtin;

import de.mknblch.sucode.interpreter.Environment;
import de.mknblch.sucode.interpreter.EvaluationException;
import de.mknblch.sucode.interpreter.Interpreter;
import de.mknblch.sucode.interpreter.TypeHelper;
import de.mknblch.sucode.interpreter.forms.Function;
import de.mknblch.sucode.parser.structs.ListStruct;

/**
 * Created by mknblch on 12.10.2014.
 */
public class StdOutForms {

    @Function(symbol = "print")
    public static Object print(ListStruct args, Environment env, Interpreter ip) throws EvaluationException {
        System.out.print(ip.eval(args.car(), env));
        return null;
    }

    @Function(symbol = "printf")
    public static Object printf(ListStruct args, Environment env, Interpreter ip) throws EvaluationException {
        final String format = TypeHelper.asString(ip.eval(args.car(), env));
        final int size = args.size()-1;
        final Object[] objects = new Object[size];
        int count = 0;
        for (Object o : args.cdr()) {
            objects[count++] = ip.eval(o, env);
        }
        System.out.printf(format, objects);
        return null;
    }
}

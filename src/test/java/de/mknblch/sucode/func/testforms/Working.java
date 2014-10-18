package de.mknblch.sucode.func.testforms;

import de.mknblch.sucode.interpreter.Context;
import de.mknblch.sucode.func.Define;
import de.mknblch.sucode.interpreter.Interpreter;
import de.mknblch.sucode.structs.ListStruct;

/**
 * Created by mknblch on 12.10.2014.
 */
public class Working {

    private static Interpreter interpreter;

    @Define
    public static Object sum (ListStruct args, Context env) {
        return 0;
    }

    @Define(symbol = "foo")
    public static Boolean t1 (ListStruct args, Context env) {
        return false;
    }

    @Define(symbol = {"bar", "baz"})
    public static Boolean t2 (ListStruct args, Context env) {
        return false;
    }
}

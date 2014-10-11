package de.mknblch.sucode.interpreter.func.testforms;

import de.mknblch.sucode.interpreter.Context;
import de.mknblch.sucode.interpreter.Interpreter;
import de.mknblch.sucode.interpreter.func.Define;
import de.mknblch.sucode.parser.structs.ListStruct;

/**
 * Created by mknblch on 12.10.2014.
 */
public class Working {

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

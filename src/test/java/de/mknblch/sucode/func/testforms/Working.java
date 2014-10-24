package de.mknblch.sucode.func.testforms;

import de.mknblch.sucode.interpreter.Context;
import de.mknblch.sucode.func.Define;
import de.mknblch.sucode.ast.ListStruct;

/**
 * @author mknblch
 */
public class Working {
    @Define(symbol = "sum")
    public static Object sum (Context env, ListStruct args) {
        return 0;
    }

    @Define(symbol = "foo")
    public static Boolean t1 (Context env, ListStruct args) {
        return false;
    }

    @Define(symbol = {"bar", "baz"})
    public static Boolean t2 (Context env, ListStruct args) {
        return false;
    }
}

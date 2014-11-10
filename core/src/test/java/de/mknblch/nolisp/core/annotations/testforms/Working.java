package de.mknblch.nolisp.core.annotations.testforms;

import de.mknblch.nolisp.core.annotations.Define;
import de.mknblch.nolisp.core.ast.ListStruct;
import de.mknblch.nolisp.core.interpreter.Context;

/**
 * @author mknblch
 */
public class Working {
    @Define(value = "sum")
    public static Object sum (Context env, ListStruct args) {
        return 0;
    }

    @Define(value = "foo")
    public static Boolean t1 (Context env, ListStruct args) {
        return false;
    }

    @Define(value = {"bar", "baz"})
    public static Boolean t2 (Context env, ListStruct args) {
        return false;
    }
}

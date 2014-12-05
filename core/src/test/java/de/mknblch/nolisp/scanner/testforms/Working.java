package de.mknblch.nolisp.scanner.testforms;

import de.mknblch.nolisp.datatypes.ListStruct;
import de.mknblch.nolisp.scanner.Define;

/**
 * @author mknblch
 */
public class Working {
    @Define(value = "sum")
    public static Object sum(ListStruct args) {
        return 0;
    }

    @Define(value = "foo")
    public static Boolean t1(ListStruct args) {
        return false;
    }

    @Define(value = {"bar", "baz"})
    public static Boolean t2(ListStruct args) {
        return false;
    }
}

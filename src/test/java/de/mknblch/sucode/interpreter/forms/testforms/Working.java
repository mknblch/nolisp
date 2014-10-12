package de.mknblch.sucode.interpreter.forms.testforms;

import de.mknblch.sucode.interpreter.Environment;
import de.mknblch.sucode.interpreter.Interpreter;
import de.mknblch.sucode.interpreter.forms.Function;
import de.mknblch.sucode.parser.structs.ListStruct;

/**
 * Created by mknblch on 12.10.2014.
 */
public class Working {

    @Function
    public static Object sum (ListStruct args, Environment env, Interpreter interpreter) {
        return 0;
    }

    @Function(symbol = "foo")
    public static Boolean t1 (ListStruct args, Environment env, Interpreter interpreter) {
        return false;
    }

    @Function(symbol = {"bar", "baz"})
    public static Boolean t2 (ListStruct args, Environment env, Interpreter interpreter) {
        return false;
    }
}

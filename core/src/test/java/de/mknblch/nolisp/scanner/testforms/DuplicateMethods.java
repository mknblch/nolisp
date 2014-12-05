package de.mknblch.nolisp.scanner.testforms;

import de.mknblch.nolisp.interpreter.Context;
import de.mknblch.nolisp.interpreter.Interpreter;
import de.mknblch.nolisp.datatypes.ListStruct;
import de.mknblch.nolisp.scanner.Define;

public class DuplicateMethods {
    @Define(value = "dup")
    public static Object wrong(ListStruct args, Context env, Interpreter interpreter) {
        return null;
    }

    @Define(value = "dup")
    public static Object wronger(ListStruct args, Context env, Interpreter interpreter) {
        return null;
    }
}

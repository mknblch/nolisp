package de.mknblch.nolisp.core.scanner.testforms;

import de.mknblch.nolisp.core.interpreter.Context;
import de.mknblch.nolisp.core.interpreter.Interpreter;
import de.mknblch.nolisp.core.datatypes.ListStruct;
import de.mknblch.nolisp.core.scanner.Define;

public class NoReturnType {

    @Define(value = "wrong")
    public static void wrong(ListStruct args, Context env, Interpreter interpreter) {
    }
}

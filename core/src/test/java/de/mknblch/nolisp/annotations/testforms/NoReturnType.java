package de.mknblch.nolisp.annotations.testforms;

import de.mknblch.nolisp.annotations.Define;
import de.mknblch.nolisp.ast.ListStruct;
import de.mknblch.nolisp.interpreter.Context;
import de.mknblch.nolisp.interpreter.Interpreter;

public class NoReturnType {

    @Define(value = "wrong")
    public static void wrong (ListStruct args, Context env, Interpreter interpreter) { }
}

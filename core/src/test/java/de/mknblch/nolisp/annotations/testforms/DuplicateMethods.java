package de.mknblch.nolisp.annotations.testforms;

import de.mknblch.nolisp.annotations.Define;
import de.mknblch.nolisp.ast.ListStruct;
import de.mknblch.nolisp.interpreter.Context;
import de.mknblch.nolisp.interpreter.Interpreter;

public class DuplicateMethods {
    @Define(value = "dup")
    public static Object wrong (ListStruct args, Context env, Interpreter interpreter) { return null; }
    @Define(value = "dup")
    public static Object wronger (ListStruct args, Context env, Interpreter interpreter) { return null; }
}

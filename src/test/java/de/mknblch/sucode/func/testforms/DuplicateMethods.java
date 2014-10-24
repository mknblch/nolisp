package de.mknblch.sucode.func.testforms;

import de.mknblch.sucode.interpreter.Context;
import de.mknblch.sucode.interpreter.Interpreter;
import de.mknblch.sucode.func.Define;
import de.mknblch.sucode.ast.ListStruct;

public class DuplicateMethods {
    @Define(value = "dup")
    public static Object wrong (ListStruct args, Context env, Interpreter interpreter) { return null; }
    @Define(value = "dup")
    public static Object wronger (ListStruct args, Context env, Interpreter interpreter) { return null; }
}

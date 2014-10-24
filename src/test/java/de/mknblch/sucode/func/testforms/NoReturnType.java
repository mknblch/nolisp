package de.mknblch.sucode.func.testforms;

import de.mknblch.sucode.interpreter.Context;
import de.mknblch.sucode.interpreter.Interpreter;
import de.mknblch.sucode.func.Define;
import de.mknblch.sucode.ast.ListStruct;

public class NoReturnType {

    @Define(value = "wrong")
    public static void wrong (ListStruct args, Context env, Interpreter interpreter) { }
}

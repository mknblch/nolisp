package de.mknblch.sucode.func.testforms;

import de.mknblch.sucode.interpreter.Context;
import de.mknblch.sucode.interpreter.Interpreter;
import de.mknblch.sucode.func.Define;
import de.mknblch.sucode.structs.ListStruct;

public class NoReturnType {
    @Define
    public static void wrong (ListStruct args, Context env, Interpreter interpreter) { }
}

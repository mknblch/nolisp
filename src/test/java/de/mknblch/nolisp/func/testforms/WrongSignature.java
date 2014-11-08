package de.mknblch.nolisp.func.testforms;

import de.mknblch.nolisp.interpreter.Context;
import de.mknblch.nolisp.func.Define;
import de.mknblch.nolisp.ast.ListStruct;

public class WrongSignature {
    @Define(value = "wrong")
    public static Object wrong (ListStruct args, Context env, String interpreter) { return null; }
}

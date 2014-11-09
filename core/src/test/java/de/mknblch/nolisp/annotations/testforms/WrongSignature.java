package de.mknblch.nolisp.annotations.testforms;

import de.mknblch.nolisp.annotations.Define;
import de.mknblch.nolisp.ast.ListStruct;
import de.mknblch.nolisp.interpreter.Context;

public class WrongSignature {
    @Define(value = "wrong")
    public static Object wrong (ListStruct args, Context env, String interpreter) { return null; }
}

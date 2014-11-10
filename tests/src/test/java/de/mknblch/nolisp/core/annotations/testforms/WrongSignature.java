package de.mknblch.nolisp.core.annotations.testforms;

import de.mknblch.nolisp.core.annotations.Define;
import de.mknblch.nolisp.core.ast.ListStruct;
import de.mknblch.nolisp.core.interpreter.Context;

public class WrongSignature {
    @Define(value = "wrong")
    public static Object wrong (ListStruct args, Context env, String interpreter) { return null; }
}

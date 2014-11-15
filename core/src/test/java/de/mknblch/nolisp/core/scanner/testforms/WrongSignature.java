package de.mknblch.nolisp.core.scanner.testforms;

import de.mknblch.nolisp.core.scanner.Define;
import de.mknblch.nolisp.core.interpreter.structs.ListStruct;
import de.mknblch.nolisp.core.interpreter.Context;

public class WrongSignature {
    @Define(value = "wrong")
    public static Object wrong (ListStruct args, Context env, String interpreter) { return null; }
}

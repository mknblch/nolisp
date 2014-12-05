package de.mknblch.nolisp.core.scanner.testforms;

import de.mknblch.nolisp.core.interpreter.Context;
import de.mknblch.nolisp.core.datatypes.ListStruct;
import de.mknblch.nolisp.core.scanner.Define;

public class WrongSignature {
    @Define(value = "wrong")
    public static Object wrong(ListStruct args, Context env, String interpreter) {
        return null;
    }
}

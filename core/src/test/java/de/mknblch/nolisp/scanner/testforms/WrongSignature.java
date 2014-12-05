package de.mknblch.nolisp.scanner.testforms;

import de.mknblch.nolisp.interpreter.Context;
import de.mknblch.nolisp.datatypes.ListStruct;
import de.mknblch.nolisp.scanner.Define;

public class WrongSignature {
    @Define(value = "wrong")
    public static Object wrong(ListStruct args, Context env, String interpreter) {
        return null;
    }
}

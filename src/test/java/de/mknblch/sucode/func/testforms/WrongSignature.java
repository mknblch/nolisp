package de.mknblch.sucode.func.testforms;

import de.mknblch.sucode.interpreter.Context;
import de.mknblch.sucode.func.Define;
import de.mknblch.sucode.structs.ListStruct;

public class WrongSignature {
    @Define(symbol = "wrong")
    public static Object wrong (ListStruct args, Context env, String interpreter) { return null; }
}

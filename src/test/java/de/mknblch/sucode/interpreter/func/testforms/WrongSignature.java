package de.mknblch.sucode.interpreter.func.testforms;

import de.mknblch.sucode.interpreter.Context;
import de.mknblch.sucode.interpreter.func.Define;
import de.mknblch.sucode.parser.structs.ListStruct;

public class WrongSignature {
    @Define
    public static Object wrong (ListStruct args, Context env, String interpreter) { return null; }
}

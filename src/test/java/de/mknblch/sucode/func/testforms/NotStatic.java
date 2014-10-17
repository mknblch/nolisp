package de.mknblch.sucode.func.testforms;

import de.mknblch.sucode.interpreter.Context;
import de.mknblch.sucode.interpreter.Interpreter;
import de.mknblch.sucode.func.Define;
import de.mknblch.sucode.structs.ListStruct;

public class NotStatic {
    @Define
    public Object wrong (ListStruct args, Context env, Interpreter interpreter) { return null; }
}

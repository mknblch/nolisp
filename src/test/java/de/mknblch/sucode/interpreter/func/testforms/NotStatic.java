package de.mknblch.sucode.interpreter.func.testforms;

import de.mknblch.sucode.interpreter.Context;
import de.mknblch.sucode.interpreter.Interpreter;
import de.mknblch.sucode.interpreter.func.Define;
import de.mknblch.sucode.parser.structs.ListStruct;

public class NotStatic {
    @Define
    public Object wrong (ListStruct args, Context env, Interpreter interpreter) { return null; }
}

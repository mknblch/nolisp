package de.mknblch.sucode.interpreter.forms;

import de.mknblch.sucode.interpreter.Environment;
import de.mknblch.sucode.interpreter.Interpreter;
import de.mknblch.sucode.parser.structs.ListStruct;

public class NotStatic {
    @Function
    public Object wrong (ListStruct args, Environment env, Interpreter interpreter) { return null; }
}

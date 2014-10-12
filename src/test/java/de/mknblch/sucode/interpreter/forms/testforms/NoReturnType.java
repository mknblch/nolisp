package de.mknblch.sucode.interpreter.forms.testforms;

import de.mknblch.sucode.interpreter.Environment;
import de.mknblch.sucode.interpreter.Interpreter;
import de.mknblch.sucode.interpreter.forms.Function;
import de.mknblch.sucode.parser.structs.ListStruct;

public class NoReturnType {
    @Function
    public static void wrong (ListStruct args, Environment env, Interpreter interpreter) { }
}

package de.mknblch.sucode.interpreter.forms.testforms;

import de.mknblch.sucode.interpreter.Environment;
import de.mknblch.sucode.interpreter.Interpreter;
import de.mknblch.sucode.interpreter.forms.Function;
import de.mknblch.sucode.parser.structs.ListStruct;

public class WrongSignature {
    @Function
    public static Object wrong (ListStruct args, Environment env, String interpreter) { return null; }
}

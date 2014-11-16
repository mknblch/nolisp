package de.mknblch.nolisp.core.scanner.testforms;

import de.mknblch.nolisp.core.interpreter.Context;
import de.mknblch.nolisp.core.interpreter.Interpreter;
import de.mknblch.nolisp.core.interpreter.structs.ListStruct;
import de.mknblch.nolisp.core.scanner.Define;

public class NotStatic {
    @Define(value = "wrong")
    public Object wrong(ListStruct args, Context env, Interpreter interpreter) {
        return null;
    }
}

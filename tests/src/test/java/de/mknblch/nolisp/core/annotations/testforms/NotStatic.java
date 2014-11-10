package de.mknblch.nolisp.core.annotations.testforms;

import de.mknblch.nolisp.core.annotations.Define;
import de.mknblch.nolisp.core.ast.ListStruct;
import de.mknblch.nolisp.core.interpreter.Interpreter;
import de.mknblch.nolisp.core.interpreter.Context;

public class NotStatic {
    @Define(value = "wrong")
    public Object wrong (ListStruct args, Context env, Interpreter interpreter) { return null; }
}

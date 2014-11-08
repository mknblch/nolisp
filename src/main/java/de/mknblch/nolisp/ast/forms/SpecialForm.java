package de.mknblch.nolisp.ast.forms;

import de.mknblch.nolisp.ast.ListStruct;
import de.mknblch.nolisp.interpreter.Context;
import de.mknblch.nolisp.interpreter.Interpreter;

/**
 * in contrast to the normal From it does not evaluate it's args before
 * calling eval. therefore the eval-function gets the interpreter
 * as an additional argument to do anything useful.
 *
 * @author mknblch
 */
public abstract class SpecialForm implements Function {

    public abstract Object eval(Interpreter interpreter, Context context, ListStruct args) throws Exception;

    @Override
    public Type getType() {
        return Type.FORM;
    }
}
package de.mknblch.sucode.ast.forms;

import de.mknblch.sucode.ast.ListStruct;
import de.mknblch.sucode.interpreter.Context;
import de.mknblch.sucode.interpreter.Interpreter;

/**
 * wrapper class for @Special-registered functions.
 * in contrast to the normal From it does not evaluate it's args before
 * calling eval. therefore the eval-function gets the interpreter
 * as an additional argument to anything useful.
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

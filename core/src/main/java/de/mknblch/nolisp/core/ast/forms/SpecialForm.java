package de.mknblch.nolisp.core.ast.forms;

import de.mknblch.nolisp.core.ast.Atom;
import de.mknblch.nolisp.core.ast.ListStruct;
import de.mknblch.nolisp.core.interpreter.Interpreter;
import de.mknblch.nolisp.core.interpreter.Context;

/**
 * in contrast to the normal From it does not evaluate it's args before
 * calling eval. therefore the eval-function gets the interpreter
 * as an additional argument to do anything useful.
 *
 * @author mknblch
 */
public interface SpecialForm extends Atom {

    public abstract Object eval(Interpreter interpreter, Context context, ListStruct args) throws Exception;
}

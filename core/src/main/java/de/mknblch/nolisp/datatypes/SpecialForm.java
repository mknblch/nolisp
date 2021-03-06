package de.mknblch.nolisp.datatypes;

import de.mknblch.nolisp.interpreter.Context;
import de.mknblch.nolisp.interpreter.Interpreter;
import de.mknblch.nolisp.datatypes.Atom;
import de.mknblch.nolisp.datatypes.ListStruct;

/**
 * in contrast to the normal From it does not evaluate it's args before
 * calling eval. therefore the eval-function gets the interpreter
 * as an additional argument to do anything useful.
 *
 * @author mknblch
 */
public interface SpecialForm { //extends Atom {

    public abstract Object eval(Interpreter interpreter, Context context, ListStruct args) throws Exception;
}

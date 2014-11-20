package de.mknblch.nolisp.core.interpreter.structs.forms;

import de.mknblch.nolisp.core.interpreter.structs.Atom;
import de.mknblch.nolisp.core.interpreter.structs.ListStruct;

/**
 * @author mknblch
 */
public interface Form extends Atom {

    public abstract Object eval(ListStruct args) throws Exception;
}

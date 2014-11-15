package de.mknblch.nolisp.core.interpreter.structs.forms;

import de.mknblch.nolisp.core.interpreter.structs.Atom;
import de.mknblch.nolisp.core.interpreter.structs.ListStruct;
import de.mknblch.nolisp.core.interpreter.Context;

/**
 * @author mknblch
 */
public interface Form extends Atom {

    public abstract Object eval(Context context, ListStruct args) throws Exception;
}

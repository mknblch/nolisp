package de.mknblch.nolisp.core.ast.forms;

import de.mknblch.nolisp.core.ast.Atom;
import de.mknblch.nolisp.core.ast.ListStruct;
import de.mknblch.nolisp.core.interpreter.Context;

/**
 * @author mknblch
 */
public interface Form extends Atom {

    public abstract Object eval(Context context, ListStruct args) throws Exception;
}

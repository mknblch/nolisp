package de.mknblch.nolisp.ast.forms;

import de.mknblch.nolisp.ast.Atom;
import de.mknblch.nolisp.ast.ListStruct;
import de.mknblch.nolisp.interpreter.Context;

/**
 * @author mknblch
 */
public interface Form extends Atom {

    public abstract Object eval(Context context, ListStruct args) throws Exception;
}

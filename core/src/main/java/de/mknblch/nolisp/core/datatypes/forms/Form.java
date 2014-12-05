package de.mknblch.nolisp.core.datatypes.forms;

import de.mknblch.nolisp.core.datatypes.Atom;
import de.mknblch.nolisp.core.datatypes.ListStruct;

/**
 * @author mknblch
 */
public interface Form extends Atom {

    public abstract Object eval(ListStruct args) throws Exception;
}

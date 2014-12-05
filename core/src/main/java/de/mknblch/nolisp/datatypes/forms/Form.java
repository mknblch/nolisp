package de.mknblch.nolisp.datatypes.forms;

import de.mknblch.nolisp.datatypes.Atom;
import de.mknblch.nolisp.datatypes.ListStruct;

/**
 * @author mknblch
 */
public interface Form extends Atom {

    public abstract Object eval(ListStruct args) throws Exception;
}

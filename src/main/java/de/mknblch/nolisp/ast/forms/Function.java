package de.mknblch.nolisp.ast.forms;

import de.mknblch.nolisp.ast.Atom;

/**
 * interface to all Forms which define their own execution rules.
 *
 * @author mknblch
 */
public interface Function extends Atom {

    /**
     * return the value used to call this function
     */
    public String getSymbol();

}

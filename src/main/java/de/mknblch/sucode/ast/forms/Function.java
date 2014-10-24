package de.mknblch.sucode.ast.forms;

import de.mknblch.sucode.ast.Atom;

/**
 * interface to all Forms which define their own execution rules.
 *
 * @author mknblch
 */
public interface Function extends Atom {

    /**
     * return the symbol used to call this function
     */
    public String getSymbol();

}

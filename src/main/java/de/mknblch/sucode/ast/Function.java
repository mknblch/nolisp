package de.mknblch.sucode.ast;

import de.mknblch.sucode.ast.Atom;

/**
 * Created by mknblch on 11.10.2014.
 */
public interface Function extends Atom {

    /**
     * return the symbol used to call this function
     */
    public String getSymbol();

}

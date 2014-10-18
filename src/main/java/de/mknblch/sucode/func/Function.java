package de.mknblch.sucode.func;

import de.mknblch.sucode.interpreter.Context;
import de.mknblch.sucode.interpreter.Interpreter;
import de.mknblch.sucode.structs.Atom;
import de.mknblch.sucode.structs.ListStruct;

/**
 * Created by mknblch on 11.10.2014.
 */
public interface Function extends Atom {

    /**
     * return the symbol used to call this function
     */
    public String getSymbol();

}

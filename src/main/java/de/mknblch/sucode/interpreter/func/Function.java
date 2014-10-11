package de.mknblch.sucode.interpreter.func;

import de.mknblch.sucode.interpreter.Context;
import de.mknblch.sucode.parser.structs.Atom;
import de.mknblch.sucode.parser.structs.ListStruct;

/**
 * Created by mknblch on 11.10.2014.
 */
public interface Function extends Atom {

    public Object eval (ListStruct args, Context context) throws Exception;

    /**
     * return the symbol used to call this function
     */
    public String getSymbol();

    /**
     * returns whether the form is as special form or not.
     * special func
     * @return
     */
    public boolean isSpecialForm();
}

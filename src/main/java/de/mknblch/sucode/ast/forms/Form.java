package de.mknblch.sucode.ast.forms;

import de.mknblch.sucode.ast.ListStruct;
import de.mknblch.sucode.interpreter.Context;

/**
 * wrapper class for functions which were registered in the context by
 * scanning for @Defined annotation.
 *
 * @author mknblch
 */
public abstract class Form implements Function {

    public abstract Object eval(Context context, ListStruct args) throws Exception;

    @Override
    public Type getType() {
        return Type.FORM;
    }
}

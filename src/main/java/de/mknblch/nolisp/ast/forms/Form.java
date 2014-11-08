package de.mknblch.nolisp.ast.forms;

import de.mknblch.nolisp.ast.ListStruct;
import de.mknblch.nolisp.interpreter.Context;

/**
 * @author mknblch
 */
public abstract class Form implements Function {

    public abstract Object eval(Context context, ListStruct args) throws Exception;

    @Override
    public Type getType() {
        return Type.FORM;
    }
}

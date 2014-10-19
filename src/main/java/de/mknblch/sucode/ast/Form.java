package de.mknblch.sucode.ast;

import de.mknblch.sucode.interpreter.Context;

/**
 * Created by mknblch on 18.10.2014.
 */
public abstract class Form implements Function {

    public abstract Object eval(Context context, ListStruct args) throws Exception;

    @Override
    public Atom.Type getType() {
        return Type.FORM;
    }
}

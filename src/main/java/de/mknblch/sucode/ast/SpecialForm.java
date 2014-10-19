package de.mknblch.sucode.ast;

import de.mknblch.sucode.interpreter.Context;
import de.mknblch.sucode.interpreter.Interpreter;

/**
 * Created by mknblch on 18.10.2014.
 */
public abstract class SpecialForm implements Function {

    public abstract Object eval(Interpreter interpreter, Context context, ListStruct args) throws Exception;

    @Override
    public Type getType() {
        return Type.SPECIAL_FORM;
    }
}

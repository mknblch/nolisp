package de.mknblch.sucode.func;

import de.mknblch.sucode.interpreter.Context;
import de.mknblch.sucode.interpreter.Interpreter;
import de.mknblch.sucode.structs.Atom;
import de.mknblch.sucode.structs.ListStruct;

/**
 * Created by mknblch on 18.10.2014.
 */
public abstract class NonSpecialForm implements Function {

    public abstract Object eval(Context context, ListStruct args) throws Exception;

    @Override
    public Atom.Type getType() {
        return Atom.Type.FUNC;
    }
}

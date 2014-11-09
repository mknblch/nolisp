package de.mknblch.nolisp.func;

import de.mknblch.nolisp.ast.forms.Form;

/**
 * @author mknblch
 */
public abstract class BuiltInForm implements Form, BuiltIn {
    @Override
    public Type getType() {
        return Type.BUILTIN;
    }
}

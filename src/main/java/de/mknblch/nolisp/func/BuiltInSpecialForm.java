package de.mknblch.nolisp.func;

import de.mknblch.nolisp.ast.forms.SpecialForm;

/**
 * @author mknblch
 */
public abstract class BuiltInSpecialForm implements SpecialForm, BuiltIn {
    @Override
    public Type getType() {
        return Type.BUILTIN;
    }
}

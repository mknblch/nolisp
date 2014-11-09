package de.mknblch.nolisp.annotations;

import de.mknblch.nolisp.ast.forms.SpecialForm;

/**
 * Adapter for built-in SpecialForm's
 * @author mknblch
 */
abstract class SpecialFormAdapter implements SpecialForm, BuiltIn {
    @Override
    public Type getType() {
        return Type.BUILTIN;
    }
}

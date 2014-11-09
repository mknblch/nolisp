package de.mknblch.nolisp.annotations;

import de.mknblch.nolisp.ast.forms.Form;

/**
 * Adapter for built-in Form's.
 * @author mknblch
 */
abstract class FormAdapter implements Form, BuiltIn {
    @Override
    public Type getType() {
        return Type.BUILTIN;
    }
}

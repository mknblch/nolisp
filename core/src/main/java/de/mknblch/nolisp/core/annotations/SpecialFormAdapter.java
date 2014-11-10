package de.mknblch.nolisp.core.annotations;

import de.mknblch.nolisp.core.ast.forms.SpecialForm;
import de.mknblch.nolisp.core.ast.Atom;

/**
 * Adapter for built-in SpecialForm's
 * @author mknblch
 */
abstract class SpecialFormAdapter implements SpecialForm, BuiltIn {
    @Override
    public Atom.Type getType() {
        return Atom.Type.BUILTIN;
    }
}

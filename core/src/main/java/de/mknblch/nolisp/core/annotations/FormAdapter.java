package de.mknblch.nolisp.core.annotations;

import de.mknblch.nolisp.core.ast.forms.Form;
import de.mknblch.nolisp.core.ast.Atom;

/**
 * Adapter for built-in Form's.
 * @author mknblch
 */
abstract class FormAdapter implements Form, BuiltIn {
    @Override
    public Atom.Type getType() {
        return Atom.Type.BUILTIN;
    }
}

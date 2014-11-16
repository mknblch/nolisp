package de.mknblch.nolisp.core.scanner;

import de.mknblch.nolisp.core.interpreter.structs.Atom;
import de.mknblch.nolisp.core.interpreter.structs.forms.SpecialForm;

/**
 * Adapter for built-in SpecialForm's
 *
 * @author mknblch
 */
abstract class SpecialFormAdapter implements SpecialForm, BuiltIn {
    @Override
    public Atom.Type getType() {
        return Atom.Type.BUILTIN;
    }
}

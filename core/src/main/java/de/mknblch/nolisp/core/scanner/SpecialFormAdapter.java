package de.mknblch.nolisp.core.scanner;

import de.mknblch.nolisp.core.interpreter.structs.forms.SpecialForm;
import de.mknblch.nolisp.core.interpreter.structs.Atom;

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

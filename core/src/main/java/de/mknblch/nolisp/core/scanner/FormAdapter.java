package de.mknblch.nolisp.core.scanner;

import de.mknblch.nolisp.core.interpreter.structs.Atom;
import de.mknblch.nolisp.core.interpreter.structs.forms.Form;

/**
 * Adapter for built-in Form's.
 *
 * @author mknblch
 */
abstract class FormAdapter implements Form, BuiltIn {
    @Override
    public Atom.Type getType() {
        return Atom.Type.BUILTIN;
    }
}

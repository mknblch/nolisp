package de.mknblch.nolisp.core.scanner;

import de.mknblch.nolisp.core.interpreter.structs.forms.Form;
import de.mknblch.nolisp.core.interpreter.structs.Atom;

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

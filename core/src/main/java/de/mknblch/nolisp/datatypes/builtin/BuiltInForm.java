package de.mknblch.nolisp.datatypes.builtin;

import de.mknblch.nolisp.datatypes.Atom;
import de.mknblch.nolisp.datatypes.Form;

/**
 * Adapter for built-in Form's.
 *
 * @author mknblch
 */
public abstract class BuiltInForm implements Form {
    @Override
    public Atom.Type getType() {
        return Atom.Type.BUILTIN;
    }
}

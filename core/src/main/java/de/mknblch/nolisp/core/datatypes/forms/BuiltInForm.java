package de.mknblch.nolisp.core.datatypes.forms;

import de.mknblch.nolisp.core.datatypes.Atom;

/**
 * Adapter for built-in Form's.
 *
 * @author mknblch
 */
public abstract class BuiltInForm implements Form, BuiltIn {
    @Override
    public Atom.Type getType() {
        return Atom.Type.BUILTIN;
    }
}
package de.mknblch.nolisp.core.datatypes.forms;

import de.mknblch.nolisp.core.datatypes.Atom;

/**
 * Adapter for built-in SpecialForm's
 *
 * @author mknblch
 */
public abstract class BuiltInSpecialForm implements SpecialForm, BuiltIn {
    @Override
    public Atom.Type getType() {
        return Atom.Type.BUILTIN;
    }
}
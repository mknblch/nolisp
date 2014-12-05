package de.mknblch.nolisp.datatypes;

/**
 * @author mknblch
 */
public interface Atom {

    public enum Type {
        SYMBOL, LIST, BUILTIN, LAMBDA, MACRO
    }

    public Type getType();
}

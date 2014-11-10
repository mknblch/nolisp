package de.mknblch.nolisp.core.ast;

/**
 *
 * @author mknblch
 */
public interface Atom {

    public enum Type {
        SYMBOL, LIST, BUILTIN, LAMBDA, MACRO
    }

    public Type getType();
}

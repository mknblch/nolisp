package de.mknblch.nolisp.ast;

/**
 *
 * @author mknblch
 */
public interface Atom {

    public enum Type {
        SYMBOL, LIST, FORM, LAMBDA, MACRO
    }

    public Type getType();
}

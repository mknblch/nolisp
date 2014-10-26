package de.mknblch.sucode.ast;

/**
 *
 * @author mknblch
 */
public interface Atom {

    public enum Type {
        SYMBOL, LIST, FORM, LAMBDA, REFERENCE, MACRO
    }

    public Type getType();
}

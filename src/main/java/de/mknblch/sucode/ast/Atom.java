package de.mknblch.sucode.ast;

/**
 *
 * @author mknblch
 */
public interface Atom {

    public enum Type {
        SYMBOL, LIST, SPECIAL_FORM, FORM
    }

    public Type getType();
}

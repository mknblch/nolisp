package de.mknblch.sucode.ast;

/**
 * Created by mknblch on 28.09.2014.
 */
public interface Atom {

    public enum Type {
        SYMBOL, LIST, SPECIAL_FORM, FORM
    }

    public Type getType();
}

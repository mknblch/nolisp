package de.mknblch.sucode.structs;

/**
 * Created by mknblch on 28.09.2014.
 */
public interface Atom {

    public enum Type {
        SYMBOL, CONST, LIST, FUNC, END
    }

    public Type getType();
}

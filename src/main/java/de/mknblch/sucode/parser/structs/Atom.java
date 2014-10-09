package de.mknblch.sucode.parser.structs;

/**
 * Created by mknblch on 28.09.2014.
 */
public interface Atom {

    public static enum Type {
        SYMBOL, INT, REAL, STRING, END, LIST
    }

    public Type getType();
}

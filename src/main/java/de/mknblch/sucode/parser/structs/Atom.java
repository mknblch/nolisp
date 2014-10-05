package de.mknblch.sucode.parser.structs;

/**
 * Created by pexx on 28.09.2014.
 */
public interface Atom {

    public static enum Type {
        SYMBOL, INT, REAL, STRING, LIST
    }

    public Type getType();
}

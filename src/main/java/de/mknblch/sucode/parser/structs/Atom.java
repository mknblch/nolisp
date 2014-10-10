package de.mknblch.sucode.parser.structs;

/**
 * Created by mknblch on 28.09.2014.
 */
public interface Atom {

    public enum Type {
        SYMBOL, CONST, LIST, END
    }

    public Type getType();
}

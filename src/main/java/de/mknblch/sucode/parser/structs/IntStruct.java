package de.mknblch.sucode.parser.structs;

/**
 * Created by pexx on 05.10.2014.
 */
public class IntStruct implements Atom {

    public final int intValue;

    public IntStruct(String literal) {
        this.intValue = Integer.parseInt(literal);
    }

    @Override
    public Type getType() {
        return Type.INT;
    }
}

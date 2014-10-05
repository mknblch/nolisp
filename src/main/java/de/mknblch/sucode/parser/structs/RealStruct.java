package de.mknblch.sucode.parser.structs;

/**
 * Created by pexx on 05.10.2014.
 */
public class RealStruct implements Atom {

    public final double realValue;

    public RealStruct(String literal) {
        this.realValue = Double.parseDouble(literal);
    }

    @Override
    public Type getType() {
        return Type.REAL;
    }
}

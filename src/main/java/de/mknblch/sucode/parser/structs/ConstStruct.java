package de.mknblch.sucode.parser.structs;

/**
 * Created by mknblch on 10.10.2014.
 */
public class ConstStruct implements Atom {

    public enum ConstType {
        INT, REAL, STRING, TRUE, NIL
    }

    public final ConstType type;
    public final Object value;

    public ConstStruct(ConstType type, Object value) {
        this.type = type;
        this.value = value;
    }

    @Override
    public Type getType() {
        return Type.CONST;
    }
}

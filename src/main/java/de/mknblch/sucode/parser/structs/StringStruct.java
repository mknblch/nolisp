package de.mknblch.sucode.parser.structs;

/**
 * Created by mknblch on 05.10.2014.
 */
public class StringStruct implements Atom {

    public final String value;

    public StringStruct(String literal) {
        this.value = literal;
    }

    @Override
    public Type getType() {
        return Type.STRING;
    }
}

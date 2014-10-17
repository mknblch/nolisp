package de.mknblch.sucode.structs;

/**
 * Created by mknblch on 05.10.2014.
 */
public class SymbolStruct implements Atom {

    public final String literal;

    public SymbolStruct(String literal) {
        this.literal = literal;
    }

    @Override
    public Type getType() {
        return Type.SYMBOL;
    }
}

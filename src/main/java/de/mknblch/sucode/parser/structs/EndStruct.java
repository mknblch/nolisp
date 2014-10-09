package de.mknblch.sucode.parser.structs;

/**
 * @author mknblch
 * @date 09.10.2014.
 */
public class EndStruct implements Atom {
    @Override
    public Type getType() {
        return Type.END;
    }
}

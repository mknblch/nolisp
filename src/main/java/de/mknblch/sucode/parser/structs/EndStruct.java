package de.mknblch.sucode.parser.structs;

/**
 * Struct which acts like a special token to signal end of list.
 * Should never be seen in parsed program.
 *
 * @author mknblch
 * @date 09.10.2014.
 */
public class EndStruct implements Atom {
    @Override
    public Type getType() {
        return Type.END;
    }
}

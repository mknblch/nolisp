package de.mknblch.sucode.ast;

/**
 * @author mknblch
 */
public class Reference implements Atom {

    public final String target;

    public Reference(String target) {
        this.target = target;
    }

    @Override
    public Type getType() {
        return Type.REFERENCE;
    }
}

package de.mknblch.nolisp.core.ast;

/**
 * @author mknblch
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

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        SymbolStruct that = (SymbolStruct) o;

        if (!literal.equals(that.literal)) return false;

        return true;
    }

    @Override
    public int hashCode() {
        return literal.hashCode();
    }
}

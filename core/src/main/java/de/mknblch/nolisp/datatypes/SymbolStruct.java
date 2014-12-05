package de.mknblch.nolisp.datatypes;

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
        return !(o == null || getClass() != o.getClass()) && literal.equals(((SymbolStruct) o).literal);
    }

    @Override
    public int hashCode() {
        return literal.hashCode();
    }
}

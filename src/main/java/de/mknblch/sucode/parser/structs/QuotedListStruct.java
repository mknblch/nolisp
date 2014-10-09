package de.mknblch.sucode.parser.structs;

/**
 * @author mknblch
 * @date 09.10.2014.
 */
public class QuotedListStruct extends ListStruct {

    public static final SymbolStruct QUOTE = new SymbolStruct("quote");

    public QuotedListStruct() {
        super(QUOTE);
    }

    public QuotedListStruct(Atom atom) {
        super(QUOTE);
        cons(atom);
    }
}

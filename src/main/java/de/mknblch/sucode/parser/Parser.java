package de.mknblch.sucode.parser;

import de.mknblch.sucode.lexer.Lexer;
import de.mknblch.sucode.lexer.LexerException;
import de.mknblch.sucode.lexer.Token;
import de.mknblch.sucode.structs.*;

import static de.mknblch.sucode.structs.ConstStruct.ConstType;

/**
 * The parser transforms a token stream into an AbstractSyntaxTree
 * <p/>
 * Created by mknblch on 05.10.2014.
 */
public class Parser {

    /**
     * this struct is used to identify the end of list.
     */
    public static final Atom END_STRUCT = new Atom() {
        @Override
        public Type getType() {
            return Type.END;
        }
    };

    public static final SymbolStruct QUOTE_STRUCT = new SymbolStruct("quote");

    public ListStruct parse(Lexer lexer) throws ParserException, LexerException {
        final ListStruct root = new ListStruct();
        while (lexer.hasNext()) {
            final Atom atom = parseOne(lexer);
            // skip comments
            if (null == atom) {
                continue;
            }
            // unbalanced count of braces found
            if (atom.getType() == Atom.Type.END) {
                throw new ParserException(String.format("[%03d] Unbalanced AST. One or more opening braces missing.", lexer.getOffset()));
            }
            root.add(atom);
        }
        return root;
    }

    private Atom parseOne(Lexer lexer) throws LexerException, ParserException {
        final Token token = lexer.next();
        switch (token.type) {
            case LIST_BEGIN:
                return parseList(lexer);
            case LIST_END:
                return END_STRUCT;
            case QUOTE:
                // expand to (quote arg)
                return new ListStruct(QUOTE_STRUCT, parseOne(lexer));
            case SYMBOL:
                return new SymbolStruct(token.literal);
            case STRING:
                return new ConstStruct(ConstType.STRING, token.value);
            case INT:
                return new ConstStruct(ConstType.INT, token.value);
            case REAL:
                return new ConstStruct(ConstType.REAL, token.value);
            case NIL:
                return new ConstStruct(ConstType.NIL, null);
            case TRUE:
                return new ConstStruct(ConstType.TRUE, null);
            case LINE_COMMENT:
                return null;
            default:
                throw new RuntimeException(String.format("[%03d] Type '%s' Not yet implemented.", lexer.getOffset(), token.type.name()));
        }
    }


    private ListStruct parseList(Lexer lexer) throws LexerException, ParserException {
        final ListStruct listStruct = new ListStruct();
        while (lexer.hasNext()) {
            final Atom atom = parseOne(lexer);
            if (atom.getType() == Atom.Type.END) {
                return listStruct;
            }
            listStruct.add(atom);
        }
        throw new ParserException(String.format("[%03d] Unbalanced AST. One or more closing braces missing.", lexer.getOffset()));
    }
}

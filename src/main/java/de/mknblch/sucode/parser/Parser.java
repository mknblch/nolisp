package de.mknblch.sucode.parser;

import de.mknblch.sucode.lexer.Lexer;
import de.mknblch.sucode.lexer.LexerException;
import de.mknblch.sucode.lexer.Token;
import de.mknblch.sucode.ast.*;

import java.util.ArrayList;
import java.util.List;

/**
 * The parser transforms a token stream into an AbstractSyntaxTree
 * <p/>
 * Created by mknblch on 05.10.2014.
 */
public class Parser {

    public static final Atom END_STRUCT = new Atom() {
        @Override
        public Type getType() {
            return null;
        }
    };
    public static final Atom COMMENT_STRUCT = new Atom() {
        @Override
        public Type getType() {
            return null;
        }
    };
    public static final SymbolStruct QUOTE_STRUCT = new SymbolStruct("quote");

    public ListStruct parse(Lexer lexer) throws ParserException, LexerException {
        final ListStruct root = new ListStruct();
        while (lexer.hasNext()) {
            final Object o = parseOne(lexer);
            if(COMMENT_STRUCT == o) continue;
            if(END_STRUCT == o) throw new ParserException(String.format("[%03d] Unbalanced AST. One or more opening braces missing.", lexer.getOffset()));
            root.add(o);
        }
        return root;
    }

    private Object parseOne(Lexer lexer) throws LexerException, ParserException {
        final Token token = lexer.next();
        switch (token.type) {

            case LIST_BEGIN: return parseList(lexer);
            case QUOTE: return new ListStruct(QUOTE_STRUCT).append(parseOne(lexer));
            case SYMBOL: return new SymbolStruct(token.literal);
            case NIL: return null; //return new ConstStruct(ConstType.NIL, null);
            case TRUE: return Boolean.TRUE; //new ConstStruct(ConstType.TRUE, null);
            case LINE_COMMENT: return COMMENT_STRUCT;
            case LIST_END: return END_STRUCT;

            case STRING:
            case INT:
            case REAL:
                return token.value;

            default:
                throw new RuntimeException(String.format("[%03d] Type '%s' Not yet implemented.", lexer.getOffset(), token.type.name()));
        }
    }


    private ListStruct parseList(Lexer lexer) throws LexerException, ParserException {
        final ListStruct listStruct = new ListStruct();
        while (lexer.hasNext()) {
            final Object o = parseOne(lexer);
            if(o == END_STRUCT) {
                return listStruct;
            }
            listStruct.add(o);
        }
        throw new ParserException(String.format("[%03d] Unbalanced AST. One or more closing braces missing.", lexer.getOffset()));
    }
}

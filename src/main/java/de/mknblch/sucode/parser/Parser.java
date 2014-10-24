package de.mknblch.sucode.parser;

import de.mknblch.sucode.parser.lexer.Lexer;
import de.mknblch.sucode.parser.lexer.LexerException;
import de.mknblch.sucode.parser.lexer.Token;
import de.mknblch.sucode.ast.*;

/**
 * The parser transforms a token stream into an AbstractSyntaxTree
 * <p/>
 * @author mknblch
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

    private final Lexer lexer = new Lexer();

    public Program parse(String code) throws ParserException, LexerException {
        lexer.setCode(code);
        Program root = new Program();
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

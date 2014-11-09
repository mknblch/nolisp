package de.mknblch.nolisp.parser;

import de.mknblch.nolisp.ast.Atom;
import de.mknblch.nolisp.ast.ListStruct;
import de.mknblch.nolisp.ast.SymbolStruct;
import de.mknblch.nolisp.parser.lexer.Lexer;
import de.mknblch.nolisp.parser.lexer.LexerException;
import de.mknblch.nolisp.parser.lexer.Token;

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

    private static final SymbolStruct QUOTE_STRUCT = new SymbolStruct("quote");
    private static final SymbolStruct FUNCTION_STRUCT = new SymbolStruct("function");
    private static final SymbolStruct BACKQUOTE_STRUCT = new SymbolStruct("backquote");
    private static final SymbolStruct COMMA_STRUCT = new SymbolStruct("comma");
    private static final SymbolStruct AT_STRUCT = new SymbolStruct("splice");

    private final Lexer lexer = new Lexer();

    public ListStruct parse(String code) throws ParserException, LexerException {
        lexer.setCode(code);
        final ListStruct program = new ListStruct();
        while (lexer.hasNext()) {
            final Object o = parseOne();
            if(COMMENT_STRUCT == o) continue;
            if(END_STRUCT == o) throw new ParserException(String.format("Unbalanced AST. One or more opening braces missing."));
            program.add(o);
        }
        return program;
    }

    private Object parseOne() throws LexerException, ParserException {
        final Token token = lexer.next();
        switch (token.type) {

            case LIST_BEGIN: return parseList();
            case LIST_END: return END_STRUCT;
            case SYMBOL: return new SymbolStruct(token.literal);
            case LINE_COMMENT: return COMMENT_STRUCT;
            case CONST: return token.value;
            // syntactic sugar
            case BACKQUOTE: return new ListStruct(BACKQUOTE_STRUCT, parseOne());
            case SHARP: return new ListStruct(FUNCTION_STRUCT, parseOne());
            case QUOTE: return new ListStruct(QUOTE_STRUCT, parseOne());
            case COMMA: return new ListStruct(COMMA_STRUCT, parseOne());
            case SPLICE: return new ListStruct(AT_STRUCT, parseOne());

            default:
                throw new ParserException(String.format("Type '%s' Not yet implemented.", token.type.name()));
        }
    }


    private ListStruct parseList() throws LexerException, ParserException {
        final ListStruct listStruct = new ListStruct();
        while (lexer.hasNext()) {
            final Object o = parseOne();
            if(o == END_STRUCT) {
                return listStruct;
            }
            listStruct.add(o);
        }
        throw new ParserException(String.format("Unbalanced AST. One or more closing braces missing."));
    }
}
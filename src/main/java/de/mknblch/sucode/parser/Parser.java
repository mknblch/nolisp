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
            if(o == COMMENT_STRUCT) continue;
            if(o == END_STRUCT) throw new ParserException(String.format("[%03d] Unbalanced AST. One or more opening braces missing.", lexer.getOffset()));
            root.add(o);
        }
        return root;
    }

    private Object parseOne(Lexer lexer) throws LexerException, ParserException {
        final Token token = lexer.next();
        switch (token.type) {
            case LIST_BEGIN: return parseList(lexer);

            case LIST_END:
                return END_STRUCT;

            // expand to (quote arg)
            case QUOTE: return new ListStruct(QUOTE_STRUCT, parseOne(lexer));

            case SYMBOL: return new SymbolStruct(token.literal);

            case STRING:
            case INT:
            case REAL:
                return token.value;

            case NIL: return new ConstStruct(ConstType.NIL, null);

            case TRUE: return Boolean.TRUE; //new ConstStruct(ConstType.TRUE, null);

            case LINE_COMMENT: return COMMENT_STRUCT;

            default:
                throw new RuntimeException(String.format("[%03d] Type '%s' Not yet implemented.", lexer.getOffset(), token.type.name()));
        }
    }


    private ListStruct parseList(Lexer lexer) throws LexerException, ParserException {
        final ListStruct listStruct = new ListStruct();
        while (lexer.hasNext()) {
            final Object o = parseOne(lexer);
            if (!(o instanceof Atom)) {
                listStruct.add(o);
                continue;
            }

            final Atom atom = ((Atom) o);
            if (atom.getType() == Atom.Type.END) {
                return listStruct;
            }
            listStruct.add(atom);
        }
        throw new ParserException(String.format("[%03d] Unbalanced AST. One or more closing braces missing.", lexer.getOffset()));
    }
}

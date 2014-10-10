package de.mknblch.sucode.parser;

import de.mknblch.sucode.lexer.Lexer;
import de.mknblch.sucode.lexer.LexerException;
import de.mknblch.sucode.lexer.Token;
import de.mknblch.sucode.parser.structs.*;
/**
 * The parser creates an AbstractSyntaxTree from a Token stream
 * Created by mknblch on 05.10.2014.
 */
public class Parser {

    public static final EndStruct END_STRUCT_STRUCT = new EndStruct();

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
                throw new ParserException("Unbalanced AST");
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
                return END_STRUCT_STRUCT;
            case QUOTE:
                return new ListStruct("quote", parseOne(lexer));
            case SYMBOL:
                return new SymbolStruct(token.literal);
            case STRING:
                return new ConstStruct(ConstStruct.ConstType.STRING, token.value);
            case INT:
                return new ConstStruct(ConstStruct.ConstType.INT, token.value);
            case REAL:
                return new ConstStruct(ConstStruct.ConstType.REAL, token.value);
            case NIL:
                return new ConstStruct(ConstStruct.ConstType.NIL, null);
            case TRUE:
                return new ConstStruct(ConstStruct.ConstType.TRUE, null);
            case LINE_COMMENT:
                return null;
            default:
                throw new RuntimeException("Not yet implemented: " + token.type);
        }
    }


    private ListStruct parseList(Lexer lexer) throws LexerException, ParserException {
        final ListStruct listStruct = new ListStruct();
        while (lexer.hasNext()) {
            Atom atom = parseOne(lexer);
            if (atom.getType() == Atom.Type.END) {
                return listStruct;
            }
            listStruct.add(atom);
        }
        throw new ParserException("Unbalanced AST");
    }
}

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

    public static final End END_STRUCT = new End();

    public Program parse(Lexer lexer) throws ParserException, LexerException {
        final Program root = new Program();
        while (lexer.hasNext()) {
            final Atom atom = parseOne(lexer);
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
            case BRACE_OPEN:
                return parseList(lexer);
            case BRACE_CLOSE:
                return END_STRUCT;
            case QUOTE:
                return new QuotedListStruct(parseOne(lexer));
            case LINE_COMMENT:
                // do nothing
                break;
            case SYMBOL:
                return new SymbolStruct(token.literal);
            case STRING:
                return new StringStruct(token.literal);
            case INT:
                return new IntStruct(token.literal);
            case REAL:
                return new RealStruct(token.literal);
        }
        throw new ParserException("Unbalanced AST");
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

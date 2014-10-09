package de.mknblch.sucode.parser;

import de.mknblch.sucode.lexer.Lexer;
import de.mknblch.sucode.lexer.LexerException;
import de.mknblch.sucode.lexer.Token;
import de.mknblch.sucode.parser.structs.*;

import java.util.List;
import java.util.Stack;

/**
 * Created by pexx on 05.10.2014.
 */
public class RDParser {

    public ListStruct parse(Lexer lexer) throws ParserException, LexerException {

        final ListStruct root = new ListStruct();

        while (lexer.hasNext()) {
            root.addCons(parseInner(lexer));
        }

        return root;
    }

    private Atom parseInner(Lexer lexer) throws LexerException, ParserException {
        Token token = lexer.next();
        System.out.println(token.type);
        switch (token.type) {

            case BRACE_OPEN:
                return parseList(lexer);
            case BRACE_CLOSE:
                return new EndStruct();

            case SYMBOL:
            case STRING:
            case INT:
            case REAL:
                return asStruct(token);

            case QUOTE:
                break;
            case LINE_COMMENT:
                break;
        }

        throw new ParserException("Bam");
    }


    private ListStruct parseList(Lexer lexer) throws LexerException, ParserException {


        ListStruct listStruct = new ListStruct();

        while (lexer.hasNext()) {

            Atom atom = parseInner(lexer);

            if (atom instanceof EndStruct) {
                return listStruct;
            }

            listStruct.addCons(atom);
        }

        throw new ParserException("Unable to parse List");
    }


    private Atom asStruct(Token token) throws ParserException {

        switch (token.type) {
            case SYMBOL:
                return new SymbolStruct(token.literal);
            case STRING:
                return new StringStruct(token.literal);
            case INT:
                return new IntStruct(token.literal);
            case REAL:
                return new RealStruct(token.literal);
        }

        throw new ParserException("Type is no ConstantStruct");
    }

    private static Atom joinQuoted(Atom atom, boolean quoted) {

        if (quoted) {
            return new QuotedListStruct(atom);
        } else {
            return atom;
        }
    }

}

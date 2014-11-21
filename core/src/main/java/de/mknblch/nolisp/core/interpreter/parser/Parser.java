package de.mknblch.nolisp.core.interpreter.parser;

import de.mknblch.nolisp.core.common.TypeHelper;
import de.mknblch.nolisp.core.inspection.Inspector;
import de.mknblch.nolisp.core.interpreter.parser.lexer.Lexer;
import de.mknblch.nolisp.core.interpreter.parser.lexer.LexerException;
import de.mknblch.nolisp.core.interpreter.parser.lexer.Token;
import de.mknblch.nolisp.core.interpreter.structs.Atom;
import de.mknblch.nolisp.core.interpreter.structs.ListStruct;
import de.mknblch.nolisp.core.interpreter.structs.SymbolStruct;

import java.io.File;
import java.io.FileInputStream;
import java.util.ArrayList;
import java.util.Scanner;

/**
 * The parser transforms a token stream into an abstract syntax tree in form of ListStructs
 * which is basically a linked list
 * <p/>
 *
 * @author mknblch
 */
public class Parser {

    private static final SpliceRule SPLICE_RULE = new SpliceRule();

    private static final Atom LIST_END_STRUCT = new Atom() {
        @Override
        public Type getType() {
            return null;
        }
    };

    private static final Atom ARRAY_END_STRUCT = new Atom() {
        @Override
        public Type getType() {
            return null;
        }
    };

    private static final Atom COMMENT_STRUCT = new Atom() {
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

    public ListStruct parse(File file) throws Exception {
        final String code = new Scanner(file).useDelimiter("\\A").next();
        return parse(code);
    }

    public ListStruct parse(String code) throws Exception {
        lexer.setCode(code);
        final ListStruct program = new ListStruct();
        while (lexer.hasNext()) {
            final Object o = parseOne();
            if (COMMENT_STRUCT == o) continue;
            if (LIST_END_STRUCT == o)
                throw new ParserException(String.format("Unbalanced AST. One or more opening braces missing."));
            if (ARRAY_END_STRUCT == o)
                throw new ParserException(String.format("Unbalanced ARRAY. One or more opening braces missing."));
            program.add(o);
        }
        return postProcesses(program);
    }

    private ListStruct postProcesses(ListStruct program) throws Exception {
        return Inspector.cloneTree(program, SPLICE_RULE);
    }

    private Object parseOne() throws LexerException, ParserException {
        final Token token = lexer.next();
        switch (token.type) {

            case LIST_BEGIN:
                return parseList();
            case LIST_END:
                return LIST_END_STRUCT;
            case ARRAY_BEGIN:
                return parseArray();
            case ARRAY_END:
                return ARRAY_END_STRUCT;
            case SYMBOL:
                return new SymbolStruct(token.literal);
            case LINE_COMMENT:
                return COMMENT_STRUCT;
            case CONST:
                return token.value;

            // syntactic sugar
            case BACKQUOTE:
                return new ListStruct(BACKQUOTE_STRUCT, parseOne());
            case SHARP:
                return new ListStruct(FUNCTION_STRUCT, parseOne());
            case QUOTE:
                return new ListStruct(QUOTE_STRUCT, parseOne());
            case COMMA:
                return new ListStruct(COMMA_STRUCT, parseOne());
            case SPLICE:
                return new ListStruct(AT_STRUCT, parseOne());

            default:
                throw new ParserException(String.format("Type '%s' Not yet implemented.", token.type.name()));
        }
    }

    private Object[] parseArray() throws ParserException, LexerException {
        final ArrayList<Object> list = new ArrayList<>();
        while (lexer.hasNext()) {
            final Object o = parseOne();
            if (o == ARRAY_END_STRUCT) {
                return list.toArray();
            }
            list.add(o);
        }
        throw new ParserException(String.format("Unbalanced ARRAY. One or more closing braces missing."));
    }


    private ListStruct parseList() throws LexerException, ParserException {
        final ListStruct listStruct = new ListStruct();
        while (lexer.hasNext()) {
            final Object o = parseOne();
            if (o == LIST_END_STRUCT) {
                return listStruct;
            }
            listStruct.add(o);
        }
        throw new ParserException(String.format("Unbalanced AST. One or more closing braces missing."));
    }
}

package de.mknblch.nolisp.parser.lexer;

/**
 * @author mknblch
 */
public class Token {

    public enum Type {
        SYMBOL, LIST_BEGIN, LIST_END, QUOTE, SHARP, BACKQUOTE, SPLICE, COMMA, LINE_COMMENT, CONST
    }

    public final Type type;
    public final String literal;
    public final Object value;

    public Token(Type type, String literal, Object value) {
        this.value = value;
        this.type = type;
        this.literal = literal;
    }
}
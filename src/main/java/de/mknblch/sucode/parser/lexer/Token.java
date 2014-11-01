package de.mknblch.sucode.parser.lexer;

/**
 * @author mknblch
 */
public class Token {

    public enum Type {
        SYMBOL, LIST_BEGIN, LIST_END, STRING, INT, REAL, TRUE, QUOTE, NIL, LINE_COMMENT, SHARP, BACKQUOTE, SPLICE, COMMA
    }

    public final Type type;
    public final String literal;
    public final Object value;
    public final int position;

    public Token(Type type, String literal, Object value, int position) {
        this.value = value;
        this.position = position;
        this.type = type;
        this.literal = literal;
    }
}

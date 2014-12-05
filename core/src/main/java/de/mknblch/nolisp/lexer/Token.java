package de.mknblch.nolisp.lexer;

/**
 * @author mknblch
 */
public class Token {

    public enum Type {
        SYMBOL, LIST_BEGIN, LIST_END, ARRAY_BEGIN, ARRAY_END, LINE_COMMENT, CONST, TRANSFORM
    }

    public final Type type;
    public final String literal;
    public final Object value;

    public Token(Type type, String literal, Object value) {
        this.type = type;
        this.literal = literal;
        this.value = value;
    }
}

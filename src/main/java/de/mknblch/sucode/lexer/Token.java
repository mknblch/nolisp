package de.mknblch.sucode.lexer;

/**
 * Created by pexx on 03.10.2014.
 */
public class Token {

    public enum Type {
        SYMBOL, BRACE_OPEN, BRACE_CLOSE, STRING, INT, REAL, QUOTE, LINE_COMMENT;
    }

    public final Type type;
    public final String literal;
    public final int position;

    public Token(int position, Type type, String literal) {
        this.position = position;
        this.type = type;
        this.literal = literal;
    }
}

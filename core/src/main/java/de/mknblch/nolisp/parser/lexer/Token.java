package de.mknblch.nolisp.parser.lexer;

/**
 * @author mknblch
 */
public class Token {

    public enum Type {
        SYMBOL,     // symbol-string
        LIST_BEGIN, // list begin terminal
        LIST_END,   // list end terminal
        ARRAY_BEGIN,// array begin terminal
        ARRAY_END,  // array end terminal
        LINE_COMMENT, // comment starting with ; and ends with newline
        CONST,      // constant like int or string
        TRANSFORM   // transformation token to initiate a special parsing rule
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

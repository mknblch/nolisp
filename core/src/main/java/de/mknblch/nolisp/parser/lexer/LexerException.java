package de.mknblch.nolisp.parser.lexer;

/**
 * @author mknblch
 */
public class LexerException extends Exception {
    public LexerException(String message) {
        super(message);
    }

    public LexerException(String message, Throwable cause) {
        super(message, cause);
    }
}

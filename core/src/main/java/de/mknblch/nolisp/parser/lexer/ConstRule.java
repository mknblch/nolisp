package de.mknblch.nolisp.parser.lexer;

/**
 * these constRules parses the literal and returns its associated token
 * or null if rule did not match
 *
 * @author mknblch
 */
public interface ConstRule {

    /**
     * return token or null
     * @param literal
     * @return
     */
    public Token token (String literal) throws LexerException;
}

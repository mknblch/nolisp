package de.mknblch.nolisp.core.interpreter.parser.lexer;

/**
 * these rules parses the literal and returns its associated token
 * or null if rule did not match
 *
 * @author mknblch
 */
public interface ConstDecisionRule {

    /**
     * return token or null
     * @param literal
     * @return
     */
    public Token token (String literal) throws LexerException;
}

package de.mknblch.nolisp.core.interpreter.parser.lexer;

/**
 * @author mknblch
 */
public interface TokenDecisionRule {

    /**
     * return token or null
     * @param literal
     * @return
     */
    public Token token (String literal) throws LexerException;
}

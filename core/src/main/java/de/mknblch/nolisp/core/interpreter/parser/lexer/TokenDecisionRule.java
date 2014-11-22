package de.mknblch.nolisp.core.interpreter.parser.lexer;

/**
 * returns the token which must be unique decidable by its
 * first character (head). therefore it must be guaranteed
 * that no operation on the lexer is performed if the first
 * char does not match! otherwise the behaviour is unpredictable.
 *
 * @author mknblch
 */
public interface TokenDecisionRule {

    public Token decide(char head, StringCutter cutter) throws LexerException;
}

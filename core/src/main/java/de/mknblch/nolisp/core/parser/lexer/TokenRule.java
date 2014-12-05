package de.mknblch.nolisp.core.parser.lexer;

/**
 * returns the token which must be clearly decidable by its
 * first character. therefore it must be guaranteed that no
 * changing operation on the lexer is performed if the first
 * char does not match! otherwise the behaviour is unpredictable.
 *
 * @author mknblch
 */
public interface TokenRule {

    public Token token(StringCutter cutter) throws LexerException;
}

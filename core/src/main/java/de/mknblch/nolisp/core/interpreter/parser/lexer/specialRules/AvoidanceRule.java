package de.mknblch.nolisp.core.interpreter.parser.lexer.specialRules;

import de.mknblch.nolisp.core.interpreter.parser.lexer.ConstRule;
import de.mknblch.nolisp.core.interpreter.parser.lexer.LexerException;
import de.mknblch.nolisp.core.interpreter.parser.lexer.Token;

/**
 * special rule which always returns a symbol token
 * @author mknblch
 */
public class AvoidanceRule implements ConstRule {

    @Override
    public Token token(String literal) throws LexerException {
        return new Token(Token.Type.SYMBOL, literal, literal);
    }
}

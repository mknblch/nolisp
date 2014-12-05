package de.mknblch.nolisp.lexer.constRules;

import de.mknblch.nolisp.lexer.LexerException;
import de.mknblch.nolisp.lexer.Token;
import de.mknblch.nolisp.lexer.ConstRule;

/**
 * @author mknblch
 */
public class NullRule implements ConstRule {

    @Override
    public Token token(String literal) throws LexerException {
        if ("null".equalsIgnoreCase(literal) || "nil".equalsIgnoreCase(literal)) {
            return new Token(Token.Type.CONST, "nil", null);
        }
        return null;
    }
}

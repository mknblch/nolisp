package de.mknblch.nolisp.core.parser.lexer.constRules;

import de.mknblch.nolisp.core.parser.lexer.LexerException;
import de.mknblch.nolisp.core.parser.lexer.Token;
import de.mknblch.nolisp.core.parser.lexer.ConstRule;

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

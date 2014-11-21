package de.mknblch.nolisp.core.interpreter.parser.lexer.rules;

import de.mknblch.nolisp.core.interpreter.parser.lexer.LexerException;
import de.mknblch.nolisp.core.interpreter.parser.lexer.Token;
import de.mknblch.nolisp.core.interpreter.parser.lexer.TokenDecisionRule;

/**
 * @author mknblch
 */
public class NullRule implements TokenDecisionRule {

    @Override
    public Token token(String literal) throws LexerException {
        if ("null".equalsIgnoreCase(literal) || "nil".equalsIgnoreCase(literal)) {
            return new Token(Token.Type.CONST, "nil", null);
        }
        return null;
    }
}

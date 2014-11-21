package de.mknblch.nolisp.core.interpreter.parser.lexer.rules;

import de.mknblch.nolisp.core.interpreter.parser.lexer.LexerException;
import de.mknblch.nolisp.core.interpreter.parser.lexer.Token;
import de.mknblch.nolisp.core.interpreter.parser.lexer.TokenDecisionRule;

/**
 * @author mknblch
 */
public class BooleanRule implements TokenDecisionRule {

    @Override
    public Token token(String literal) throws LexerException {
        if ("true".equalsIgnoreCase(literal)) {
            return new Token(Token.Type.CONST, "true", Boolean.TRUE);
        }
        if ("false".equalsIgnoreCase(literal)) {
            return new Token(Token.Type.CONST, "false", Boolean.FALSE);
        }
        return null;
    }
}

package de.mknblch.nolisp.core.interpreter.parser.lexer.rules;

import de.mknblch.nolisp.core.interpreter.parser.lexer.LexerException;
import de.mknblch.nolisp.core.interpreter.parser.lexer.Token;
import de.mknblch.nolisp.core.interpreter.parser.lexer.TokenDecisionRule;

/**
 * @author mknblch
 */
public class JavaPrimitivesRule implements TokenDecisionRule {

    @Override
    public Token token(String literal) throws LexerException {

        if ("int".equalsIgnoreCase(literal)) {
            return new Token(Token.Type.CONST, "INT", Integer.TYPE);
        }
        if ("double".equalsIgnoreCase(literal)) {
            return new Token(Token.Type.CONST, "DOUBLE", Double.TYPE);
        }
        if ("boolean".equalsIgnoreCase(literal)) {
            return new Token(Token.Type.CONST, "BOOLEAN", Boolean.TYPE);
        }
        if ("long".equalsIgnoreCase(literal)) {
            return new Token(Token.Type.CONST, "LONG", Long.TYPE);
        }
        if ("float".equalsIgnoreCase(literal)) {
            return new Token(Token.Type.CONST, "FLOAT", Float.TYPE);
        }
        if ("byte".equalsIgnoreCase(literal)) {
            return new Token(Token.Type.CONST, "BYTE", Byte.TYPE);
        }
        if ("short".equalsIgnoreCase(literal)) {
            return new Token(Token.Type.CONST, "SHORT", Short.TYPE);
        }
        if ("char".equalsIgnoreCase(literal)) {
            return new Token(Token.Type.CONST, "CHAR", Character.TYPE);
        }
        return null;
    }
}

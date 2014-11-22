package de.mknblch.nolisp.core.interpreter.parser.lexer.rules;

import de.mknblch.nolisp.core.interpreter.parser.lexer.StringCutter;
import de.mknblch.nolisp.core.interpreter.parser.lexer.Token;
import de.mknblch.nolisp.core.interpreter.parser.lexer.TokenDecisionRule;

/**
 * @author mknblch
 */
public class CommentRule implements TokenDecisionRule {

    private static final char[] NEWLINE_CHARS = new char[]{'\n'};

    @Override
    public Token decide(char head, StringCutter cutter) {

        if (';' != head) return null;

        cutter.until(NEWLINE_CHARS);
        final String literal = cutter.getLiteral();
        return new Token(Token.Type.LINE_COMMENT, literal, literal);
    }
}

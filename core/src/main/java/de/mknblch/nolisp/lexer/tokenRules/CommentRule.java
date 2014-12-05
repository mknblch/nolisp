package de.mknblch.nolisp.lexer.tokenRules;

import de.mknblch.nolisp.lexer.StringCutter;
import de.mknblch.nolisp.lexer.Token;
import de.mknblch.nolisp.lexer.TokenRule;

/**
 * @author mknblch
 */
public class CommentRule implements TokenRule {

    private static final char[] NEWLINE_CHARS = new char[]{'\n'};

    @Override
    public Token token(StringCutter cutter) {

        if (';' != cutter.charAtOffset()) return null;

        cutter.inc();
        cutter.until(NEWLINE_CHARS);
        final String literal = cutter.getLiteral();
        return new Token(Token.Type.LINE_COMMENT, literal, literal);
    }
}

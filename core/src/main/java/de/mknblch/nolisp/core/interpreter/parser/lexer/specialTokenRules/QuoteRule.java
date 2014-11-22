package de.mknblch.nolisp.core.interpreter.parser.lexer.specialTokenRules;

import de.mknblch.nolisp.core.interpreter.parser.lexer.StringCutter;
import de.mknblch.nolisp.core.interpreter.parser.lexer.Token;
import de.mknblch.nolisp.core.interpreter.parser.lexer.TokenRule;

/**
 * @author mknblch
 */
public class QuoteRule implements TokenRule {
    @Override
    public Token token(StringCutter cutter) {

        switch (cutter.charAtOffset()) {
            case '\'':
                cutter.inc();
                return new Token(Token.Type.QUOTE, "'", "'");
            case '`':
                cutter.inc();
                return new Token(Token.Type.BACKQUOTE, "`", "`");
            case ',':
                cutter.inc();
                return new Token(Token.Type.COMMA, ",", ",");
            case '.':
                cutter.inc();
                return new Token(Token.Type.SPLICE, ".", ".");

            default: return null;
        }
    }
}

package de.mknblch.nolisp.core.interpreter.parser.lexer.rules;

import de.mknblch.nolisp.core.interpreter.parser.lexer.StringCutter;
import de.mknblch.nolisp.core.interpreter.parser.lexer.Token;
import de.mknblch.nolisp.core.interpreter.parser.lexer.TokenDecisionRule;

/**
 * @author mknblch
 */
public class SpecialTokenRule implements TokenDecisionRule {
    @Override
    public Token decide(char head, StringCutter cutter) {

        switch (head) {
            case '\'':
                return new Token(Token.Type.QUOTE, "'", "'");
            case '#':
                return new Token(Token.Type.SHARP, "#", "#");
            case '`':
                return new Token(Token.Type.BACKQUOTE, "`", "`");
            case ',':
                return new Token(Token.Type.COMMA, ",", ",");
            case '.':
                return new Token(Token.Type.SPLICE, ".", ".");
            case '@':
                return new Token(Token.Type.SPLICE, "@", "@");

            default: return null;
        }
    }
}

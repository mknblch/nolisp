package de.mknblch.nolisp.core.interpreter.parser.lexer.specialTokenRules;

import de.mknblch.nolisp.core.interpreter.parser.lexer.StringCutter;
import de.mknblch.nolisp.core.interpreter.parser.lexer.Token;
import de.mknblch.nolisp.core.interpreter.parser.lexer.TokenRule;

/**
 * @author mknblch
 */
public class ListRule implements TokenRule {
    @Override
    public Token token(StringCutter cutter) {

        switch (cutter.charAtOffset()) {
            case '(':
                cutter.inc();
                return new Token(Token.Type.LIST_BEGIN, "(", "(");
            case ')':
                cutter.inc();
                return new Token(Token.Type.LIST_END, ")", ")");
            case '{':
                cutter.inc();
                return new Token(Token.Type.LIST_BEGIN, "{", "{");
            case '}':
                cutter.inc();
                return new Token(Token.Type.LIST_END, "}", "}");

            default: return null;
        }
    }
}

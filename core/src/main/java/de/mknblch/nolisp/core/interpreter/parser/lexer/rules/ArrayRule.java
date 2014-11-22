package de.mknblch.nolisp.core.interpreter.parser.lexer.rules;

import de.mknblch.nolisp.core.interpreter.parser.lexer.StringCutter;
import de.mknblch.nolisp.core.interpreter.parser.lexer.Token;
import de.mknblch.nolisp.core.interpreter.parser.lexer.TokenDecisionRule;

/**
 * @author mknblch
 */
public class ArrayRule implements TokenDecisionRule {
    @Override
    public Token decide(char head, StringCutter cutter) {

        switch (head) {
            case '[':
                return new Token(Token.Type.ARRAY_BEGIN, "[", "[");
            case ']':
                return new Token(Token.Type.ARRAY_END, "]", "]");

            default: return null;
        }
    }
}

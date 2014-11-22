package de.mknblch.nolisp.core.interpreter.parser.lexer.tokenRules;

import de.mknblch.nolisp.core.interpreter.parser.lexer.StringCutter;
import de.mknblch.nolisp.core.interpreter.parser.lexer.Token;
import de.mknblch.nolisp.core.interpreter.parser.lexer.TokenRule;

/**
 * @author mknblch
 */
public class ArrayRule implements TokenRule {
    @Override
    public Token token(StringCutter cutter) {

        switch (cutter.charAtOffset()) {
            case '[':
                cutter.inc();
                return new Token(Token.Type.ARRAY_BEGIN, "[", "[");
            case ']':
                cutter.inc();
                return new Token(Token.Type.ARRAY_END, "]", "]");

            default: return null;
        }
    }
}
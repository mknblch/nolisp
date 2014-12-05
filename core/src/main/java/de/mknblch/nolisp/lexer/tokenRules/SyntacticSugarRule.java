package de.mknblch.nolisp.lexer.tokenRules;

import de.mknblch.nolisp.lexer.StringCutter;
import de.mknblch.nolisp.lexer.Token;
import de.mknblch.nolisp.lexer.TokenRule;

/**
 * @author mknblch
 */
public class SyntacticSugarRule implements TokenRule {
    @Override
    public Token token(StringCutter cutter) {

        if(cutter.lookAheadEquals(new char[]{',', '.'}) || cutter.lookAheadEquals(new char[]{',', '@'})) {
            cutter.inc(2);
            return new Token(Token.Type.TRANSFORM, "comma-splice", ",@");
        }

        switch (cutter.charAtOffset()) {
            case '#':
                cutter.inc();
                return new Token(Token.Type.TRANSFORM, "function", "#");
            case '@':
                cutter.inc();
                return new Token(Token.Type.TRANSFORM, "splice", "@");
            case '.':
                cutter.inc();
                return new Token(Token.Type.TRANSFORM, "splice", ".");
            case '\'':
                cutter.inc();
                return new Token(Token.Type.TRANSFORM, "quote", "'");
            case '`':
                cutter.inc();
                return new Token(Token.Type.TRANSFORM, "backquote", "`");
            case ',':
                cutter.inc();
                return new Token(Token.Type.TRANSFORM, "comma", ",");

            default: return null;
        }
    }
}

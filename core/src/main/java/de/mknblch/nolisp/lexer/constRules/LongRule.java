package de.mknblch.nolisp.lexer.constRules;

import de.mknblch.nolisp.lexer.Token;

import java.util.regex.Matcher;

/**
 * @author mknblch
 */
public class LongRule extends ConstRulePatternAdapter {

    public LongRule() {
        super("^(\\-?[0-9]+)[Ll]$");
    }

    @Override
    protected Token construct(String literal, Matcher matcher) {
        return new Token(Token.Type.CONST, literal, Long.parseLong(matcher.group(1)));
    }
}

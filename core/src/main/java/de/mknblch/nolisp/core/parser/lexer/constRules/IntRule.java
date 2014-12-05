package de.mknblch.nolisp.core.parser.lexer.constRules;

import de.mknblch.nolisp.core.parser.lexer.Token;

import java.util.regex.Matcher;

/**
 * @author mknblch
 */
public class IntRule extends ConstRulePatternAdapter {

    public IntRule() {
        super("^\\-?[0-9]+$");
    }

    @Override
    protected Token construct(String literal, Matcher matcher) {
        return new Token(Token.Type.CONST, literal, Integer.parseInt(literal));
    }
}

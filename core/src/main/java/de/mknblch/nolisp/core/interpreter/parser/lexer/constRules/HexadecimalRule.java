package de.mknblch.nolisp.core.interpreter.parser.lexer.constRules;

import de.mknblch.nolisp.core.interpreter.parser.lexer.Token;

import java.util.regex.Matcher;

/**
 * @author mknblch
 */
public class HexadecimalRule extends ConstRulePatternAdapter {

    public HexadecimalRule() {
        super("^(0x)([0-9a-fA-F]+)$");
    }

    @Override
    protected Token construct(String literal, Matcher matcher) {
        return new Token(Token.Type.CONST, literal, Integer.parseInt(matcher.group(2), 16));
    }
}

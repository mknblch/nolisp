package de.mknblch.nolisp.parser.lexer.rules;

import de.mknblch.nolisp.parser.lexer.ConstRulePatternAdapter;
import de.mknblch.nolisp.parser.lexer.Token;

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

package de.mknblch.nolisp.parser.lexer.rules;

import de.mknblch.nolisp.parser.lexer.ConstRulePatternAdapter;
import de.mknblch.nolisp.parser.lexer.Token;

import java.util.regex.Matcher;

/**
 * @author mknblch
 */
public class RealRule extends ConstRulePatternAdapter {

    public RealRule() {
        super("^\\-?[0-9]+\\.[0-9]+$");
    }

    @Override
    protected Token construct(String literal, Matcher matcher) {
        return new Token(Token.Type.CONST, literal, Double.parseDouble(literal));
    }
}

package de.mknblch.nolisp.core.interpreter.parser.lexer.rules;

import de.mknblch.nolisp.core.interpreter.parser.lexer.ConstDecisionRuleAdapter;
import de.mknblch.nolisp.core.interpreter.parser.lexer.Token;

import java.math.BigDecimal;
import java.util.regex.Matcher;

/**
 * @author mknblch
 */
public class BigDecimalRule extends ConstDecisionRuleAdapter {

    public BigDecimalRule() {
        super("^(\\-?[0-9]+(\\.[0-9]+)?)[dD]?$");
    }

    @Override
    protected Token construct(String literal, Matcher matcher) {
        return new Token(Token.Type.CONST, literal, new BigDecimal(matcher.group(1)));
    }
}

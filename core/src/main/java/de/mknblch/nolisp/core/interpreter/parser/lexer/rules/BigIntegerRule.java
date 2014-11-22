package de.mknblch.nolisp.core.interpreter.parser.lexer.rules;

import de.mknblch.nolisp.core.interpreter.parser.lexer.ConstDecisionRuleAdapter;
import de.mknblch.nolisp.core.interpreter.parser.lexer.Token;

import java.math.BigInteger;
import java.util.regex.Matcher;

/**
 * @author mknblch
 */
public class BigIntegerRule extends ConstDecisionRuleAdapter {

    public BigIntegerRule() {
        super("^(\\-?[0-9]+)[bB]$");
    }

    @Override
    protected Token construct(String literal, Matcher matcher) {
        return new Token(Token.Type.CONST, literal, new BigInteger(matcher.group(1)));
    }
}
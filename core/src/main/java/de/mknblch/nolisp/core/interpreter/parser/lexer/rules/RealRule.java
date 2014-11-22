package de.mknblch.nolisp.core.interpreter.parser.lexer.rules;

import de.mknblch.nolisp.core.interpreter.parser.lexer.ConstDecisionRuleAdapter;
import de.mknblch.nolisp.core.interpreter.parser.lexer.Token;

import java.util.regex.Matcher;

/**
 * @author mknblch
 */
public class RealRule extends ConstDecisionRuleAdapter {

    public RealRule() {
        super("^\\-?[0-9]+\\.[0-9]+$");
    }

    @Override
    protected Token construct(String literal, Matcher matcher) {
        return new Token(Token.Type.CONST, literal, Double.parseDouble(literal));
    }
}

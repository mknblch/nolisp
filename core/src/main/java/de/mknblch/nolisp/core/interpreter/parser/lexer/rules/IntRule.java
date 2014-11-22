package de.mknblch.nolisp.core.interpreter.parser.lexer.rules;

import de.mknblch.nolisp.core.interpreter.parser.lexer.ConstDecisionRuleAdapter;
import de.mknblch.nolisp.core.interpreter.parser.lexer.Token;

import java.util.regex.Matcher;

/**
 * @author mknblch
 */
public class IntRule extends ConstDecisionRuleAdapter {

    public IntRule() {
        super("^\\-?[0-9]+$");
    }

    @Override
    protected Token construct(String literal, Matcher matcher) {
        return new Token(Token.Type.CONST, literal, Integer.parseInt(literal));
    }
}

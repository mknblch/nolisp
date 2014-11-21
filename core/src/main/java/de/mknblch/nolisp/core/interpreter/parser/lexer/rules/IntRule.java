package de.mknblch.nolisp.core.interpreter.parser.lexer.rules;

import de.mknblch.nolisp.core.interpreter.parser.lexer.PatternDecisionRule;
import de.mknblch.nolisp.core.interpreter.parser.lexer.Token;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * @author mknblch
 */
public class IntRule extends PatternDecisionRule {

    public IntRule() {
        super("^\\-?[0-9]+$");
    }

    @Override
    protected Token construct(String literal, Matcher matcher) {
        return new Token(Token.Type.CONST, literal, Integer.parseInt(literal));
    }
}

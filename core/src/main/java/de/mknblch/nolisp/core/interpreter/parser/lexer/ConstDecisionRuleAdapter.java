package de.mknblch.nolisp.core.interpreter.parser.lexer;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * @author mknblch
 */
public abstract class ConstDecisionRuleAdapter implements ConstDecisionRule {

    private final Pattern pattern;

    public ConstDecisionRuleAdapter(String regex) {
        this.pattern = Pattern.compile(regex);
    }

    @Override
    public Token token(String literal) throws LexerException {
        final Matcher matcher = pattern.matcher(literal);
        if(!matcher.matches()) return null;
        return construct(literal, matcher);
    }

    protected abstract Token construct (String literal, Matcher matcher) ;
}
